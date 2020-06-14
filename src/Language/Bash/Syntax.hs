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

prettyHeredocs :: [BashSyn Redir] -> Doc ann
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
prettyBlockList :: Doc ann -> BashSyn List -> Doc ann -> Doc ann -> Doc ann -> Doc ann
prettyBlockList pre l bs block be
    | hasHeredoc l = pre <+> pretty l $+$ bs $+$ block $+$ be
    | otherwise    = prettyBlock pre (pretty l) bs block be

-- | Does the last statement in a list have a here doc attached?
hasHeredoc :: BashSyn List -> Bool
hasHeredoc (List () []) = False
hasHeredoc (List () xs) = let
    Statement () l _ = last xs
    BashDoc _ _ hds = toBashDoc l
    in case hds of
        Empty -> False
        _     -> True
hasHeredoc x = shouldNeverHappen "hasHeredoc" x

data Command deriving Data
data ShellCommand deriving Data
data Redir deriving Data
data Assign deriving Data
data AssignBuiltin
data List deriving Data
data WordList deriving Data
data CaseClause deriving Data
data CaseTerm deriving Data
data IODesc deriving Data
data RedirOp deriving Data
data HeredocOp deriving Data
data Statement deriving Data
data ListTerm deriving Data
data AndOr deriving Data
data Pipeline deriving Data
data AssignOp deriving Data
data RValue deriving Data

type family Ann t a where
    Ann t t = ()
    Ann _ _ = Void

data BashSyn t
      -- | A Bash command with redirections.
    = Command (Ann Command t) (BashSyn ShellCommand) [BashSyn Redir]

      -- | A simple command consisting of assignments followed by words.
    | SimpleCommand (Ann ShellCommand t) [BashSyn Assign] [Word.Word]
      -- | The shell builtins @declare@, @eval@, @export@, @local@, @readonly@,
      -- and @typeset@ can accept both assignments and words as arguments.
    | AssignBuiltin (Ann ShellCommand t) Word.Word [Either (BashSyn Assign) Word.Word]
      -- | A function name and definition.
    | FunctionDef (Ann ShellCommand t) String (BashSyn List)  -- TODO: BashSynify function name
      -- | A named coprocess.
    | Coproc (Ann ShellCommand t) String (BashSyn Command)  -- TODO: BashSynify coproc name (?)
      -- | A @(...)@ list, denoting a subshell.
    | Subshell (Ann ShellCommand t) (BashSyn List)
      -- | A @{...}@ list.
    | Group (Ann ShellCommand t) (BashSyn List)
      -- | An arithmetic expression.
    | Arith (Ann ShellCommand t) String
      -- | A Bash @[[...]]@ conditional expression.
    | Cond (Ann ShellCommand t) (CondExpr Word.Word)
      -- | A @for /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | For (Ann ShellCommand t) String (BashSyn WordList) (BashSyn List)  -- TODO: BashSynify name
      -- | An arithmetic @for ((...))@ command.
    | ArithFor (Ann ShellCommand t) String (BashSyn List)  -- TODO: BashSynify name
      -- | A @select /name/ in /words/@ command. If @in /words/@ is absent,
      -- the word list defaults to @\"$\@\"@.
    | Select (Ann ShellCommand t) String (BashSyn WordList) (BashSyn List)  -- TODO: BashSynify name
      -- | A @case@ command.
    | Case (Ann ShellCommand t) Word.Word [BashSyn CaseClause]
      -- | An @if@ command, with a predicate, consequent, and alternative.
      -- @elif@ clauses are parsed as nested @if@ statements.
    | If (Ann ShellCommand t) (BashSyn List) (BashSyn List) (Maybe (BashSyn List))
      -- | An @until@ command.
    | Until (Ann ShellCommand t) (BashSyn List) (BashSyn List)
      -- | A @while@ command.
    | While (Ann ShellCommand t) (BashSyn List) (BashSyn List)

      -- | The arguments of a function/programm: @\"$\@\"@.
    | Args (Ann WordList t)
      -- | A word list.
    | WordList (Ann WordList t) [Word.Word]

      -- | A single case clause.
    | CaseClause (Ann CaseClause t) [Word.Word] (BashSyn List) (BashSyn CaseTerm)

      -- | The case clause terminator @;;@.
    | Break (Ann CaseTerm t)
      -- | The case clause terminator @;&@.
    | FallThrough (Ann CaseTerm t)
      -- | The case clause terminator @;;&@.
    | Continue (Ann CaseTerm t)

      -- | A redirection.
    | Redir (Ann Redir t) (Maybe (BashSyn IODesc)) (BashSyn RedirOp) Word.Word
      -- | A here document.
    | Heredoc (Ann Redir t) (BashSyn HeredocOp) String Bool Word.Word  -- TODO: BashSynify heredoc delimiter

      -- | A file descriptor number.
    | IONumber (Ann IODesc t) Int
      -- | A variable @{/varname/}@ to allocate a file descriptor for.
    | IOVar (Ann IODesc t) String

      -- | The redirection operator @\<@.
    | In (Ann RedirOp t)
      -- | The redirection operator @\>@.
    | Out (Ann RedirOp t)
      -- | The redirection operator @\>|@.
    | OutOr (Ann RedirOp t)
      -- | The redirection operator @\>\>@.
    | Append (Ann RedirOp t)
      -- | The redirection operator @&\>@.
    | AndOut (Ann RedirOp t)
      -- | The redirection operator @&\>\>@.
    | AndAppend (Ann RedirOp t)
      -- | The redirection operator @\<\<\<@.
    | HereString (Ann RedirOp t)
      -- | The redirection operator @\<&@.
    | InAnd (Ann RedirOp t)
      -- | The redirection operator @\>&@.
    | OutAnd (Ann RedirOp t)
      -- | The redirection operator @\<\>@.
    | InOut (Ann RedirOp t)

      -- | The here document operator @\<\<@.
    | Here (Ann HeredocOp t)
      -- | The here document operator @\<\<-@.
    | HereStrip (Ann HeredocOp t)

      -- | A compound list of statements.
    | List (Ann List t) [BashSyn Statement]

      -- | A single statement in a list.
    | Statement (Ann Statement t) (BashSyn AndOr) (BashSyn ListTerm)

      -- | The statement terminator @;@.
    | Sequential (Ann ListTerm t)
      -- | The statement terminator @&@.
    | Asynchronous (Ann ListTerm t)

      -- A right-associative list of pipelines.
      -- | The last pipeline of a list.
    | Last (Ann AndOr t) (BashSyn Pipeline)
      -- | A @&&@ construct.
    | And (Ann AndOr t) (BashSyn Pipeline) (BashSyn AndOr)
      -- | A @||@ construct.
    | Or (Ann AndOr t) (BashSyn Pipeline) (BashSyn AndOr)

      {- | A (possibly timed or inverted) pipeline, linked with @|@ or @|&@.
           'True' if the pipeline is timed with @time@.
           'True' if the pipeline is timed with the @-p@ flag.
           'True' if the pipeline is inverted with @!@.
           A list of commands, separated by @|@, or @|&@.
           @command1 |& command2@ is treated as a shorthand for
           @command1 2>&1 | command2@.
      -}
    | Pipeline (Ann Pipeline t) Bool Bool Bool [BashSyn Command]  -- TODO: BashSynify time, -p, !

      -- | An assignment.
    | Assign (Ann Assign t) Word.Parameter (BashSyn AssignOp) (BashSyn RValue)

      -- | An assignment operator @=@.
    | Equals (Ann AssignOp t)
      -- | An assignment operator @+=@.
    | PlusEquals (Ann AssignOp t)

      -- | The right side of an assignment: A simple word.
    | RValue (Ann RValue t) Word.Word
      -- | The right side of an assignment: An array assignment, as @(subscript, word)@ pairs.
    | RArray (Ann RValue t) [(Maybe Word.Word, Word.Word)]

deriving instance (ForallAnnC Data t, Data t, Typeable t) => Data (BashSyn t)
deriving instance ForallAnnC Eq t => Eq (BashSyn t)
deriving instance ForallAnnC Read t => Read (BashSyn t)
deriving instance ForallAnnC Show t => Show (BashSyn t)
deriving instance Typeable (BashSyn t)
deriving instance Generic (BashSyn t)

--deriving instance Ord (BashSyn CaseTerm)
--deriving instance Bounded (BashSyn CaseTerm)
--deriving instance Enum (BashSyn CaseTerm)
-- RedirOp: deriving (Ord, Enum, Bounded)
-- HeredocOp: deriving (Ord, Enum, Bounded)
-- ListTerm: deriving (Ord, Bounded, Enum)
-- AssignOp: deriving (Ord, Bounded, Enum)
-- CondExpr: deriving (Functor, Foldable, Traversable)      -- | The arguments of a function/programm: @\"$\@\"@.

type ForallAnnC (c :: * -> Constraint) t =
    ( c (Ann AndOr t)
    , c (Ann Assign t)
    , c (Ann AssignOp t)
    , c (Ann CaseClause t)
    , c (Ann CaseTerm t)
    , c (Ann Command t)
    , c (Ann HeredocOp t)
    , c (Ann IODesc t)
    , c (Ann List t)
    , c (Ann ListTerm t)
    , c (Ann Pipeline t)
    , c (Ann Redir t)
    , c (Ann RedirOp t)
    , c (Ann RValue t)
    , c (Ann ShellCommand t)
    , c (Ann Statement t)
    , c (Ann WordList t)
    --
    , c (Ann Word.Parameter t)
    , c (Ann Word.Word t)
    )

--      -- | A redirection.
--    | Redir
--        { -- | An optional file descriptor.
--          redirDesc   :: Maybe IODesc
--          -- | The redirection operator.
--        , redirOp     :: RedirOp
--          -- | The redirection target.
--        , redirTarget :: Word
--        }
--      -- | A here document.
--    | Heredoc
--        { -- | The here document operator.
--          heredocOp          :: HeredocOp
--          -- | The here document delimiter.
--        , heredocDelim       :: String
--          -- | 'True' if the delimiter was quoted.
--        , heredocDelimQuoted :: Bool
--          -- | The document itself, if the delimiter was quoted, no expansions
--          -- are parsed. If the delimiter was not quoted, parameter, arithmetic
--          -- and command substitutions take place.
--        , hereDocument       :: Word
--        }

--    { -- | 'True' if the pipeline is timed with @time@.
--      timed      :: Bool
--      -- | 'True' if the pipeline is timed with the @-p@ flag.
--    , timedPosix :: Bool
--      -- | 'True' if the pipeline is inverted with @!@.
--    , inverted   :: Bool
--      -- | A list of commands, separated by @|@, or @|&@.
--      -- @command1 |& command2@ is treated as a shorthand for
--      -- @command1 2>&1 | command2@.
--    , commands   :: [Command]
--    } deriving (Data, Eq, Read, Show, Typeable, Generic)

instance Pretty (BashSyn Command) where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc (BashSyn Command) where
    toBashDoc (Command () c rs) = BashDoc mempty (pretty c <++> pretty rs) (prettyHeredocs $ filter isHeredoc rs)
        where
            isHeredoc (Heredoc () _ _ _ _) = True
            isHeredoc _ = False
    toBashDoc x = shouldNeverHappen "ToBashDoc" x

instance Pretty (BashSyn ShellCommand) where
    pretty (SimpleCommand () as ws)  = pretty as <++> pretty ws
    pretty (AssignBuiltin () w args) = pretty w <++> hsep (map (either pretty pretty) args)
    pretty (FunctionDef () name l) =
        pretty name <+> "()" $+$ pretty (Group () l :: BashSyn ShellCommand)
    pretty (Coproc () name c) =
        "coproc" <+> pretty name <+> pretty c
    pretty (Subshell () l) =
        "(" <+> pretty l <+> ")"
    pretty (Group () l) =
        "{" $+$ indent' (pretty l) $+$ "}"
    pretty (Arith () s) =
        "((" <> pretty s <> "))"
    pretty (Cond () e) =
        "[[" <+> pretty e <+> "]]"
    pretty (For () w ws l) =
        prettyBlock "for" (pretty w <+> pretty ws <> ";") "do" (indent' $ pretty l) "done"
    pretty (ArithFor () s l) =
        prettyBlock "for" ("((" <> pretty s <> "))") "do" (indent' $ pretty l) "done"
    pretty (Select () w ws l) =
        prettyBlock "select" (pretty w <++> pretty ws <> ";") "do" (indent' $ pretty l) "done"
    pretty (Case () w cs) =
        prettyBlock "case" (pretty w) "in" (vcat $ map (indent' . pretty) cs) "esac"
    pretty (If () p t f) =
        prettyBlockList "if" p "then"
        (indent' (pretty t) $++$ (maybe mempty (\l -> "else" $+$ indent' (pretty l)) f)
        )
        "fi"
    pretty (Until () p l) =
        prettyBlockList "until" p "do" (indent' $ pretty l) "done"
    pretty (While () p l) =
        prettyBlockList "while" p "do" (indent' $ pretty l) "done"
    pretty x = shouldNeverHappen "Pretty" x

instance Pretty (BashSyn WordList) where
    pretty (Args ())        = mempty
    pretty (WordList () ws) = "in" <+> pretty ws
    pretty x                = shouldNeverHappen "Pretty" x

instance Pretty (BashSyn CaseClause) where
    pretty (CaseClause () ps l term) =
        hcat (punctuate " | " (map pretty ps)) <> ")" $+$
        indent' (pretty l) $+$
        (indent' $ pretty term)
    pretty x = shouldNeverHappen "Pretty" x

instance Operator (BashSyn CaseTerm) where
    operatorTable =
        [ (Break ()      , ";;" )
        , (FallThrough (), ";&" )
        , (Continue ()   , ";;&")
        ]

instance Pretty (BashSyn CaseTerm) where
    pretty = prettyOperator

instance Pretty (BashSyn Redir) where
    pretty (Redir () redirDesc redirOp redirTarget) =
        pretty redirDesc <> pretty redirOp <> pretty redirTarget
    pretty (Heredoc () heredocOp heredocDelim heredocDelimQuoted _) =
        pretty heredocOp <>
        pretty (if heredocDelimQuoted
              then "'" ++ heredocDelim ++ "'"
              else heredocDelim)
    pretty x = shouldNeverHappen "Pretty" x

    prettyList = hsep . map pretty

instance Pretty (BashSyn IODesc) where
    pretty (IONumber () n) = pretty n
    pretty (IOVar () n)    = "{" <> pretty n <> "}"
    pretty x               = shouldNeverHappen "Pretty" x

instance Operator (BashSyn RedirOp) where
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

instance Pretty (BashSyn RedirOp) where
    pretty = prettyOperator

instance Operator (BashSyn HeredocOp) where
    operatorTable =
        [ (Here ()     , "<<" )
        , (HereStrip (), "<<-")
        ]

instance Pretty (BashSyn HeredocOp) where
    pretty = prettyOperator

instance Pretty (BashSyn List) where
    pretty (List () as) = pretty as
    pretty x            = shouldNeverHappen "Pretty" x

instance Pretty (BashSyn Statement) where
    pretty = prettyBashDoc . toBashDoc

    prettyList = foldr f mempty
      where
        f a@(Statement () _ (Sequential ()))   b = pretty a $++$ b
        f a@(Statement () _ (Asynchronous ())) b = pretty a <++> b
        f x                                    _ = shouldNeverHappen "Pretty" x

instance ToBashDoc (BashSyn Statement) where
    toBashDoc (Statement () l lt) = toBashDoc l <> toBashDoc lt
    toBashDoc x                   = shouldNeverHappen "ToBashDoc" x

instance Operator (BashSyn ListTerm) where
    operatorTable =
        [ (Sequential ()  , ";" )
        , (Sequential ()  , "\n")
        , (Asynchronous (), "&" )
        ]

instance Pretty (BashSyn ListTerm) where
    pretty = prettyOperator

instance ToBashDoc (BashSyn ListTerm) where
    toBashDoc (Sequential ())   = docOp ";"
    toBashDoc (Asynchronous ()) = docOp "&"
    toBashDoc x                 = shouldNeverHappen "ToBashDoc" x

instance Pretty (BashSyn AndOr) where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc (BashSyn AndOr) where
    toBashDoc (Last () p)  = toBashDoc p
    toBashDoc (And () p a) = toBashDoc p <> docOp " &&" <> toBashDoc a
    toBashDoc (Or () p a)  = toBashDoc p <> docOp " ||" <> toBashDoc a
    toBashDoc x            = shouldNeverHappen "ToBashDoc" x

instance Pretty (BashSyn Pipeline) where
    pretty = prettyBashDoc . toBashDoc

instance ToBashDoc (BashSyn Pipeline) where
    toBashDoc (Pipeline () timed timedPosix inverted commands) = let
        timed'      = if timed      then "time" else mempty
        timedPosix' = if timedPosix then "-p"   else mempty
        inverted'   = if inverted   then "!"    else mempty
        space       = if timed || timedPosix || inverted then " " else mempty
        prefix = BashDoc mempty (timed' <++> timedPosix' <++> inverted' <> space) mempty
        in prefix <> mconcat (intersperse (docOp " |") (map toBashDoc commands))
    toBashDoc x = shouldNeverHappen "ToBashDoc" x

instance Pretty (BashSyn Assign) where
    pretty (Assign () lhs op rhs) = pretty lhs <> pretty op <> pretty rhs
    pretty x                      = shouldNeverHappen "Pretty" x

    prettyList = hsep . map pretty

instance Operator (BashSyn AssignOp) where
    operatorTable =
        [ (Equals ()    , "=" )
        , (PlusEquals (), "+=")
        ]

instance Pretty (BashSyn AssignOp) where
    pretty = prettyOperator

instance Pretty (BashSyn RValue) where
    pretty (RValue () w)  = pretty w
    pretty (RArray () rs) = "(" <> hsep (map f rs) <> ")"
      where
        f (Nothing , w) = pretty w
        f (Just sub, w) = "[" <> pretty sub <> "]=" <> pretty w
    pretty x = shouldNeverHappen "Pretty" x



shouldNeverHappen :: (Show a, Typeable a) => String -> a -> b
shouldNeverHappen cls x = error $ "SHOULD NEVER HAPPEN: " ++ cls ++ "[" ++ show (typeOf x) ++ "]: " ++ show x
