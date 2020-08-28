{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shell script types.
module Language.Bash.Syntax where
--    (
--      -- * Commands
--      Command(..)
--    , ShellCommand(..)
--    , WordList(..)
--    , CaseClause(..)
--    , CaseTerm(..)
--      -- * Redirections
--    , Redir(..)
--    , IODesc(..)
--    , RedirOp(..)
--    , HeredocOp(..)
--      -- * Lists
--    , List(..)
--    , Statement(..)
--    , ListTerm(..)
--    , AndOr(..)
--    , Pipeline(..)
--      -- * Assignments
--    , Assign(..)
--    , AssignOp(..)
--    , RValue(..)
--    ) where

import Prelude hiding (Word)

import Data.Data        (Data)
import Data.Kind        (Constraint)
import Data.List        (intersperse)
import Data.Semigroup   (Semigroup(..))
import Data.Typeable    (Typeable, typeOf)
import Data.Void        (Void)
import GHC.Generics     (Generic)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>), hardline, hcat, hsep, indent, nest, nesting, punctuate, vcat)
import Data.Text.Prettyprint.Doc.Internal (Doc(Empty))

import Language.Bash.Cond (CondExpr)
import Language.Bash.Operator
import Language.Bash.Pretty
import qualified Language.Bash.Word as Word

-- | The BashDoc monoid is used for building Statements, AndOr or Pipelines.
-- Consider the following situation: We have the following command
--
-- > cat <<EOF
-- > some here doc
-- > EOF
--
-- and we want to pipe its output to another arbitrary command @cmd@.
-- We want this pipeline to look like this:
--
-- > cat <<EOF |
-- > some here doc
-- > EOF
-- > cmd
--
-- Note the @|@ at the end of the first line: If we were simply pretty printing the @cat@ command we had no idea where to insert the pipe symbol.
-- And that's the purpose of BashDoc: We store possible suffixes to such lines, commands and the here documents attached to them separately and do the concatenation in the Semigroup instance of BashDoc.
data BashDoc ann = BashDoc
    (Doc ann) -- ^ The head: This is stuff we want to put before the line break and here documents
    (Doc ann) -- ^ The tail: Everthing which follows the here documents
    (Doc ann) -- ^ Collected here documents

instance Semigroup (BashDoc ann) where
    BashDoc Empty Empty Empty <> y = y
    x <> BashDoc Empty Empty Empty = x
    BashDoc h1 t1 Empty <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> h2 <++> t2) hds2
    BashDoc h1 t1 hds1  <> BashDoc h2 t2 hds2 = BashDoc h1 (t1 <> noIndent (h2 $++$ hds1) $++$ t2) hds2
        where
            noIndent doc = nesting $ \i -> nest (- i) doc

instance Monoid (BashDoc ann) where
    mempty = BashDoc mempty mempty mempty
    mappend = (<>)

docOp :: Doc ann -> BashDoc ann
docOp xs = BashDoc xs mempty mempty

prettyBashDoc :: BashDoc ann -> Doc ann
prettyBashDoc (BashDoc h t hds) = h <++> t $++$ hds

-- | A utility class for pretty printing without heredocs
class ToBashDoc a where
    toBashDoc :: a -> BashDoc ann

prettyHeredocs :: [BashSyn ann Redir] -> Doc ann'
prettyHeredocs [] = mempty
prettyHeredocs rs = mconcat $ intersperse hardline $ map prettyHeredoc rs
    where
        prettyHeredoc (Heredoc _ _ heredocDelim _ hereDocument) = pretty hereDocument <> pretty heredocDelim
        prettyHeredoc _ = mempty

-- | Indent by 4 columns.
indent' :: Doc ann -> Doc ann
indent' = indent 4

-- | Render a conditional command with a block.
prettyBlock :: Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
prettyBlock pre cond bs block be = pre <+> cond <+> bs $+$ block $+$ be

-- | Render a conditional command with a block whose condition is a list of statements.
prettyBlockList :: (Show ann, Typeable ann) => Doc ann' -> BashSyn ann List -> Doc ann' -> Doc ann' -> Doc ann' -> Doc ann'
prettyBlockList pre l bs block be
    | hasHeredoc l = pre <+> pretty l $+$ bs $+$ block $+$ be
    | otherwise    = prettyBlock pre (pretty l) bs block be

-- | Does the last statement in a list have a here doc attached?
hasHeredoc :: (Show ann, Typeable ann) => BashSyn ann List -> Bool
hasHeredoc (List _ []) = False
hasHeredoc (List _ xs) = let
    Statement _ l _ = last xs
    BashDoc _ _ hds = toBashDoc l
    in case hds of
        Empty -> False
        _     -> True
hasHeredoc x = shouldNeverHappen "hasHeredoc" x

data Command deriving Data
data ShellCommand deriving Data
data WordList deriving Data
data CaseClause deriving Data
data CaseTerm deriving Data
data Redir deriving Data
data IODesc deriving Data
data RedirOp deriving Data
data HeredocOp deriving Data
data List deriving Data
data Statement deriving Data
data ListTerm deriving Data
data AndOr deriving Data
data Pipeline deriving Data
data Assign deriving Data
data AssignOp deriving Data
data RValue deriving Data

type family Ann ann t a where
    Ann ann t t = ann
    Ann _   _ _ = Void

data BashSyn ann t
      -- | A Bash command with redirections.
    = Command (Ann ann Command t) (BashSyn ann ShellCommand) [BashSyn ann Redir]

      -- | A simple command consisting of assignments followed by words.
    | SimpleCommand (Ann ann ShellCommand t) [BashSyn ann Assign] [Word.Word]
      -- | The shell builtins @declare@, @eval@, @export@, @local@, @readonly@,
      -- and @typeset@ can accept both assignments and words as arguments.
    | AssignBuiltin (Ann ann ShellCommand t) Word.Word [Either (BashSyn ann Assign) Word.Word]
      -- | A function name and definition.
    | FunctionDef (Ann ann ShellCommand t) String (BashSyn ann List)  -- TODO: BashSynify function name
      -- | A named coprocess.
    | Coproc (Ann ann ShellCommand t) String (BashSyn ann Command)  -- TODO: BashSynify coproc name (?)
      -- | A @(...)@ list, denoting a subshell.
    | Subshell (Ann ann ShellCommand t) (BashSyn ann List)
      -- | A @{...}@ list.
    | Group (Ann ann ShellCommand t) (BashSyn ann List)
      -- | An arithmetic expression.
    | Arith (Ann ann ShellCommand t) String
      -- | A Bash @[[...]]@ conditional expression.
    | Cond (Ann ann ShellCommand t) (CondExpr Word.Word)
      -- | A @for /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | For (Ann ann ShellCommand t) String (BashSyn ann WordList) (BashSyn ann List)  -- TODO: BashSynify name
      -- | An arithmetic @for ((...))@ command.
    | ArithFor (Ann ann ShellCommand t) String (BashSyn ann List)  -- TODO: BashSynify name
      -- | A @select /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | Select (Ann ann ShellCommand t) String (BashSyn ann WordList) (BashSyn ann List)  -- TODO: BashSynify name
      -- | A @case@ command.
    | Case (Ann ann ShellCommand t) Word.Word [BashSyn ann CaseClause]
      -- | An @if@ command, with a predicate, consequent, and alternative.
      -- @elif@ clauses are parsed as nested @if@ statements.
    | If (Ann ann ShellCommand t) (BashSyn ann List) (BashSyn ann List) (Maybe (BashSyn ann List))
      -- | An @until@ command.
    | Until (Ann ann ShellCommand t) (BashSyn ann List) (BashSyn ann List)
      -- | A @while@ command.
    | While (Ann ann ShellCommand t) (BashSyn ann List) (BashSyn ann List)

      -- | The arguments of a function/programm: @\"$\@\"@.
    | Args (Ann ann WordList t)
      -- | A word list.
    | WordList (Ann ann WordList t) [Word.Word]

      -- | A single case clause.
    | CaseClause (Ann ann CaseClause t) [Word.Word] (BashSyn ann List) (BashSyn ann CaseTerm)

      -- | The case clause terminator @;;@.
    | Break (Ann ann CaseTerm t)
      -- | The case clause terminator @;&@.
    | FallThrough (Ann ann CaseTerm t)
      -- | The case clause terminator @;;&@.
    | Continue (Ann ann CaseTerm t)

      -- | A redirection.
    | Redir (Ann ann Redir t) (Maybe (BashSyn ann IODesc)) (BashSyn ann RedirOp) Word.Word
      -- | A here document.
    | Heredoc (Ann ann Redir t) (BashSyn ann HeredocOp) String Bool Word.Word  -- TODO: BashSynify heredoc delimiter

      -- | A file descriptor number.
    | IONumber (Ann ann IODesc t) Int
      -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar (Ann ann IODesc t) String

      -- | The redirection operator @\<@.
    | In (Ann ann RedirOp t)
      -- | The redirection operator @\>@.
    | Out (Ann ann RedirOp t)
      -- | The redirection operator @\>|@.
    | OutOr (Ann ann RedirOp t)
      -- | The redirection operator @\>\>@.
    | Append (Ann ann RedirOp t)
      -- | The redirection operator @&\>@.
    | AndOut (Ann ann RedirOp t)
      -- | The redirection operator @&\>\>@.
    | AndAppend (Ann ann RedirOp t)
      -- | The redirection operator @\<\<\<@.
    | HereString (Ann ann RedirOp t)
      -- | The redirection operator @\<&@.
    | InAnd (Ann ann RedirOp t)
      -- | The redirection operator @\>&@.
    | OutAnd (Ann ann RedirOp t)
      -- | The redirection operator @\<\>@.
    | InOut (Ann ann RedirOp t)

      -- | The here document operator @\<\<@.
    | Here (Ann ann HeredocOp t)
      -- | The here document operator @\<\<-@.
    | HereStrip (Ann ann HeredocOp t)

      -- | A compound list of statements.
    | List (Ann ann List t) [BashSyn ann Statement]

      -- | A single statement in a list.
    | Statement (Ann ann Statement t) (BashSyn ann AndOr) (BashSyn ann ListTerm)

      -- | The statement terminator @;@.
    | Sequential (Ann ann ListTerm t)
      -- | The statement terminator @&@.
    | Asynchronous (Ann ann ListTerm t)

      -- A right-associative list of pipelines.
      -- | The last pipeline of a list.
    | Last (Ann ann AndOr t) (BashSyn ann Pipeline)
      -- | A @&&@ construct.
    | And (Ann ann AndOr t) (BashSyn ann Pipeline) (BashSyn ann AndOr)
      -- | A @||@ construct.
    | Or (Ann ann AndOr t) (BashSyn ann Pipeline) (BashSyn ann AndOr)

      {- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
           'True' if the pipeline is timed with @time@.
           'True' if the pipeline is timed with the @-p@ flag.
           'True' if the pipeline is inverted with @!@.
           A list of commands, separated by @|@, or @|&@.
           @command1 |& command2@ is treated as a shorthand for
           @command1 2>&1 | command2@.
      -}
    | Pipeline (Ann ann Pipeline t) Bool Bool Bool [BashSyn ann Command]  -- TODO: BashSynify time, -p, !

      -- | An assignment.
    | Assign (Ann ann Assign t) Word.Parameter (BashSyn ann AssignOp) (BashSyn ann RValue)

      -- | An assignment operator @=@.
    | Equals (Ann ann AssignOp t)
      -- | An assignment operator @+=@.
    | PlusEquals (Ann ann AssignOp t)

      -- | The right side of an assignment: A simple word.
    | RValue (Ann ann RValue t) Word.Word
      -- | The right side of an assignment: An array assignment, as @(subscript, word)@ pairs.
    | RArray (Ann ann RValue t) [(Maybe Word.Word, Word.Word)]

deriving instance (ForallAnnC Data ann t, Data ann, Data t, Typeable  ann, Typeable t) => Data (BashSyn ann t)
deriving instance ForallAnnC Eq ann t => Eq (BashSyn ann t)
deriving instance ForallAnnC Read ann t => Read (BashSyn ann t)
deriving instance ForallAnnC Show ann t => Show (BashSyn ann t)
deriving instance Typeable (BashSyn ann t)
deriving instance Generic (BashSyn ann t)

-- CaseTerm: deriving (Ord, Bounded, Enum)
-- RedirOp: deriving (Ord, Enum, Bounded)
-- HeredocOp: deriving (Ord, Enum, Bounded)
-- ListTerm: deriving (Ord, Bounded, Enum)
-- AssignOp: deriving (Ord, Bounded, Enum)

type ForallAnnC (c :: * -> Constraint) ann t =
    ( c ann
    , c (Ann ann AndOr t)
    , c (Ann ann Assign t)
    , c (Ann ann AssignOp t)
    , c (Ann ann CaseClause t)
    , c (Ann ann CaseTerm t)
    , c (Ann ann Command t)
    , c (Ann ann HeredocOp t)
    , c (Ann ann IODesc t)
    , c (Ann ann List t)
    , c (Ann ann ListTerm t)
    , c (Ann ann Pipeline t)
    , c (Ann ann Redir t)
    , c (Ann ann RedirOp t)
    , c (Ann ann RValue t)
    , c (Ann ann ShellCommand t)
    , c (Ann ann Statement t)
    , c (Ann ann WordList t)
    )

instance (Show ann, Typeable ann) => Pretty (BashSyn ann Command) where
    pretty = prettyBashDoc . toBashDoc

instance (Show ann, Typeable ann) => ToBashDoc (BashSyn ann Command) where
    toBashDoc (Command _ c rs) = BashDoc mempty (pretty c <++> pretty rs) (prettyHeredocs $ filter isHeredoc rs)
        where
            isHeredoc Heredoc {} = True
            isHeredoc _ = False
    toBashDoc x = shouldNeverHappen "ToBashDoc" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann ShellCommand) where
    pretty (SimpleCommand _ as ws)  = pretty as <++> pretty ws
    pretty (AssignBuiltin _ w args) = pretty w <++> hsep (map (either pretty pretty) args)
    pretty (FunctionDef _ name l) =
        pretty name <+> "()" $+$ "{" $+$ indent' (pretty l) $+$ "}"
    pretty (Coproc _ name c) =
        "coproc" <+> pretty name <+> pretty c
    pretty (Subshell _ l) =
        "(" <+> pretty l <+> ")"
    pretty (Group _ l) =
        "{" $+$ indent' (pretty l) $+$ "}"
    pretty (Arith _ s) =
        "((" <> pretty s <> "))"
    pretty (Cond _ e) =
        "[[" <+> pretty e <+> "]]"
    pretty (For _ w ws l) =
        prettyBlock "for" (pretty w <+> pretty ws <> ";") "do" (indent' $ pretty l) "done"
    pretty (ArithFor _ s l) =
        prettyBlock "for" ("((" <> pretty s <> "))") "do" (indent' $ pretty l) "done"
    pretty (Select _ w ws l) =
        prettyBlock "select" (pretty w <++> pretty ws <> ";") "do" (indent' $ pretty l) "done"
    pretty (Case _ w cs) =
        prettyBlock "case" (pretty w) "in" (vcat $ map (indent' . pretty) cs) "esac"
    pretty (If _ p t f) =
        prettyBlockList "if" p "then"
        (indent' (pretty t) $++$ maybe mempty (\l -> "else" $+$ indent' (pretty l)) f
        )
        "fi"
    pretty (Until _ p l) =
        prettyBlockList "until" p "do" (indent' $ pretty l) "done"
    pretty (While _ p l) =
        prettyBlockList "while" p "do" (indent' $ pretty l) "done"
    pretty x = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann WordList) where
    pretty (Args _)        = mempty
    pretty (WordList _ ws) = "in" <+> pretty ws
    pretty x               = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann CaseClause) where
    pretty (CaseClause _ ps l term) =
        hcat (punctuate " | " (map pretty ps)) <> ")" $+$
        indent' (pretty l) $+$
        indent' (pretty term)
    pretty x = shouldNeverHappen "Pretty" x

instance Operator (BashSyn () CaseTerm) where
    operatorTable =
        [ (Break ()      , ";;" )
        , (FallThrough (), ";&" )
        , (Continue ()   , ";;&")
        ]

instance (Show ann, Typeable ann) => Pretty (BashSyn ann CaseTerm) where
    pretty (Break _)       = prettyOperator (Break () :: BashSyn () CaseTerm)
    pretty (FallThrough _) = prettyOperator (FallThrough () :: BashSyn () CaseTerm)
    pretty (Continue _)    = prettyOperator (Continue () :: BashSyn () CaseTerm)
    pretty x               = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann Redir) where
    pretty (Redir _ redirDesc redirOp redirTarget) =
        pretty redirDesc <> pretty redirOp <> pretty redirTarget
    pretty (Heredoc _ heredocOp heredocDelim heredocDelimQuoted _) =
        pretty heredocOp <>
        pretty (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim)
    pretty x = shouldNeverHappen "Pretty" x

    prettyList = hsep . map pretty

instance (Show ann, Typeable ann) => Pretty (BashSyn ann IODesc) where
    pretty (IONumber _ n) = pretty n
    pretty (IOVar _ n)    = "{" <> pretty n <> "}"
    pretty x              = shouldNeverHappen "Pretty" x

instance Operator (BashSyn () RedirOp) where
    operatorTable =
        [ (In ()        , "<"  )
        , (Out ()       , ">"  )
        , (OutOr ()     , ">|" )
        , (Append ()    , ">>" )
        , (AndOut ()    , "&>" )
        , (AndAppend () , "&>>")
        , (HereString (), "<<<")
        , (InAnd ()     , "<&" )
        , (OutAnd ()    , ">&" )
        , (InOut ()     , "<>" )
        ]

instance (Show ann, Typeable ann) => Pretty (BashSyn ann RedirOp) where
    pretty (In _)         = prettyOperator (In () :: BashSyn () RedirOp)
    pretty (Out _)        = prettyOperator (Out () :: BashSyn () RedirOp)
    pretty (OutOr _)      = prettyOperator (OutOr () :: BashSyn () RedirOp)
    pretty (Append _)     = prettyOperator (Append () :: BashSyn () RedirOp)
    pretty (AndOut _)     = prettyOperator (AndOut () :: BashSyn () RedirOp)
    pretty (AndAppend _)  = prettyOperator (AndAppend () :: BashSyn () RedirOp)
    pretty (HereString _) = prettyOperator (HereString () :: BashSyn () RedirOp)
    pretty (InAnd _)      = prettyOperator (InAnd () :: BashSyn () RedirOp)
    pretty (OutAnd _)     = prettyOperator (OutAnd () :: BashSyn () RedirOp)
    pretty (InOut _)      = prettyOperator (InOut () :: BashSyn () RedirOp)
    pretty x              = shouldNeverHappen "Pretty" x

instance Operator (BashSyn () HeredocOp) where
    operatorTable =
        [ (Here ()     , "<<" )
        , (HereStrip (), "<<-")
        ]

instance (Show ann, Typeable ann) => Pretty (BashSyn ann HeredocOp) where
    pretty (Here _)      = prettyOperator (Here () :: BashSyn () HeredocOp)
    pretty (HereStrip _) = prettyOperator (HereStrip () :: BashSyn () HeredocOp)
    pretty x             = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann List) where
    pretty (List _ as) = pretty as
    pretty x           = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann Statement) where
    pretty = prettyBashDoc . toBashDoc

    prettyList = foldr f mempty
      where
        f a@(Statement _ _ (Sequential _))   b = pretty a $++$ b
        f a@(Statement _ _ (Asynchronous _)) b = pretty a <++> b
        f x                                  _ = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => ToBashDoc (BashSyn ann Statement) where
    toBashDoc (Statement _ l lt) = toBashDoc l <> toBashDoc lt
    toBashDoc x                  = shouldNeverHappen "ToBashDoc" x

instance Operator (BashSyn () ListTerm) where
    operatorTable =
        [ (Sequential ()  , ";" )
        , (Sequential ()  , "\n")
        , (Asynchronous (), "&" )
        ]

instance (Show ann, Typeable ann) => Pretty (BashSyn ann ListTerm) where
    pretty (Sequential _)   = prettyOperator (Sequential () :: BashSyn () ListTerm)
    pretty (Asynchronous _) = prettyOperator (Asynchronous () :: BashSyn () ListTerm)
    pretty x                = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => ToBashDoc (BashSyn ann ListTerm) where
    toBashDoc (Sequential _)   = docOp ";"
    toBashDoc (Asynchronous _) = docOp "&"
    toBashDoc x                = shouldNeverHappen "ToBashDoc" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann AndOr) where
    pretty = prettyBashDoc . toBashDoc

instance (Show ann, Typeable ann) => ToBashDoc (BashSyn ann AndOr) where
    toBashDoc (Last _ p)  = toBashDoc p
    toBashDoc (And _ p a) = toBashDoc p <> docOp " &&" <> toBashDoc a
    toBashDoc (Or _ p a)  = toBashDoc p <> docOp " ||" <> toBashDoc a
    toBashDoc x           = shouldNeverHappen "ToBashDoc" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann Pipeline) where
    pretty = prettyBashDoc . toBashDoc

instance (Show ann, Typeable ann) => ToBashDoc (BashSyn ann Pipeline) where
    toBashDoc (Pipeline _ timed timedPosix inverted commands) = let
        timed'      = if timed      then "time" else mempty
        timedPosix' = if timedPosix then "-p"   else mempty
        inverted'   = if inverted   then "!"    else mempty
        space       = if timed || timedPosix || inverted then " " else mempty
        prefix = BashDoc mempty (timed' <++> timedPosix' <++> inverted' <> space) mempty
        in prefix <> mconcat (intersperse (docOp " |") (map toBashDoc commands))
    toBashDoc x = shouldNeverHappen "ToBashDoc" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann Assign) where
    pretty (Assign _ lhs op rhs) = pretty lhs <> pretty op <> pretty rhs
    pretty x                     = shouldNeverHappen "Pretty" x

    prettyList = hsep . map pretty

instance Operator (BashSyn () AssignOp) where
    operatorTable =
        [ (Equals ()    , "=" )
        , (PlusEquals (), "+=")
        ]

instance (Show ann, Typeable ann) => Pretty (BashSyn ann AssignOp) where
    pretty (Equals _)     = prettyOperator (Equals () :: BashSyn () AssignOp)
    pretty (PlusEquals _) = prettyOperator (PlusEquals () :: BashSyn () AssignOp)
    pretty x              = shouldNeverHappen "Pretty" x

instance (Show ann, Typeable ann) => Pretty (BashSyn ann RValue) where
    pretty (RValue _ w)  = pretty w
    pretty (RArray _ rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (Nothing , w) = pretty w
        f (Just sub, w) = "[" <> pretty sub <> "]=" <> pretty w
    pretty x = shouldNeverHappen "Pretty" x



shouldNeverHappen :: (Show a, Typeable a) => String -> a -> b
shouldNeverHappen cls x = error $ "SHOULD NEVER HAPPEN: " ++ cls ++ "[" ++ show (typeOf x) ++ "]: " ++ show x
