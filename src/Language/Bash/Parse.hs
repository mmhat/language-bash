{-# LANGUAGE RecordWildCards #-}
-- | Bash script and input parsing.
module Language.Bash.Parse
    ( parse
    ) where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Either
import           Data.Functor.Identity
import           Text.Parsec.Char             hiding (newline)
import           Text.Parsec.Combinator       hiding (optional)
import           Text.Parsec.Error            (ParseError)
import           Text.Parsec.Expr
import           Text.Parsec.Pos
import           Text.Parsec.Prim             hiding (parse, (<|>))

import qualified Language.Bash.Cond           as Cond
import           Language.Bash.Operator
import           Language.Bash.Parse.Internal
import           Language.Bash.Syntax
import           Language.Bash.Word           (unquote, stringToWord)

-- | User state.
data U = U { postHeredoc :: Maybe (State D U) }

-- | Bash parser type.
type Parser = ParsecT D U Identity

-- | Parse a script or input line into a (possibly empty) list of commands.
parse :: SourceName -> String -> Either ParseError (BashSyn () List)
parse source = runParser script (U Nothing) source . pack (initialPos source)

-------------------------------------------------------------------------------
-- Basic parsers
-------------------------------------------------------------------------------

infixl 3 </>
infix  0 ?:

-- | Backtracking choice.
(</>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p </> q = try p <|> q

-- | Name a parser from the front.
(?:) :: String -> ParsecT s u m a -> ParsecT s u m a
(?:) = flip (<?>)

-- | Parse the next here document.
heredoc :: Bool -> String -> Parser String
heredoc strip end = "here document" ?: do
    (h, s) <- lookAhead duck
    setState $ U (Just s)
    return h
  where
    process = if strip then dropWhile (== '\t') else id

    duck = do
        u <- getState
        case postHeredoc u of
            Nothing -> () <$ line
            Just s  -> () <$ setParserState s
        h <- unlines <$> heredocLines
        s <- getParserState
        return (h, s)

    line = many (satisfy (/= '\n')) <* optional (char '\n')

    heredocLines = [] <$ eof
               <|> nextLine

    nextLine = do
        l <- process <$> line
        if l == end
            then return []
            else (l :) <$> heredocLines

-- | Parse a newline, skipping any here documents.
newline :: Parser String
newline = "newline" ?: do
    _ <- operator "\r\n" <|> operator "\n"
    u <- getState
    case postHeredoc u of
        Nothing -> return ()
        Just s  -> () <$ setParserState s
    setState $ U Nothing
    skipSpace
    return "\n"

-- | Parse a list terminator.
listTerm :: Parser (BashSyn () ListTerm)
listTerm = term <* newlineList <?> "list terminator"
  where
    term = Sequential ()   <$ newline
       <|> Sequential ()   <$ operator ";"
       <|> Asynchronous () <$ operator "&"

-- | Skip zero or more newlines.
newlineList :: Parser ()
newlineList = skipMany newline

-------------------------------------------------------------------------------
-- Simple commands
-------------------------------------------------------------------------------

-- | Skip a redirection.
redir :: Parser (BashSyn () Redir)
redir = normalRedir
    <|> heredocRedir
    <?> "redirection"
  where
    normalRedir :: Parser (BashSyn () Redir)
    normalRedir = Redir ()
        <$> optional ioDesc
        <*> redirOperator
        <*> anyWord

    heredocRedir :: Parser (BashSyn () Redir)
    heredocRedir = do
        heredocOp <- heredocOperator
        w <- anyWord
        let heredocDelim = unquote w
            heredocDelimQuoted = stringToWord heredocDelim /= w
        h <- heredoc (heredocOp == HereStrip ()) heredocDelim
        hereDocument <- if heredocDelimQuoted
                        then return (stringToWord h)
                        else heredocWord h
        return $ Heredoc ()
            heredocOp
            heredocDelim
            heredocDelimQuoted
            hereDocument

    redirOperator   = selectOperator operator <?> "redirection operator"
    heredocOperator = selectOperator operator <?> "here document operator"

-- | Parse a list of redirections.
redirList :: Parser [BashSyn () Redir]
redirList = many redir

-- | Parse part of a command.
commandParts :: Parser a -> Parser ([a], [BashSyn () Redir])
commandParts p = partitionEithers <$> many part
  where
    part = Left  <$> p
       <|> Right <$> redir

-- | Parse a simple command.
simpleCommand :: Parser (BashSyn () Command)
simpleCommand = do
    notFollowedBy reservedWord
    assignCommand </> normalCommand
  where
    assignCommand = "assignment builtin" ?: do
        rs1 <- redirList
        w <- assignBuiltin
        (args, rs2) <- commandParts assignArg
        return $ Command () (AssignBuiltin () w args) (rs1 ++ rs2)

    normalCommand = "simple command" ?: do
        (as, rs1) <- commandParts assign
        (ws, rs2) <- commandParts anyWord
        let rs = rs1 ++ rs2
        guard (not $ null as && null ws && null rs)
        return $ Command () (SimpleCommand () as ws) rs

    assignArg = Left  <$> assign
            <|> Right <$> anyWord

-------------------------------------------------------------------------------
-- Lists
-------------------------------------------------------------------------------

-- | A list with one command.
singleton :: BashSyn () ShellCommand -> BashSyn () List
singleton c =
    List () [Statement () (Last () (unmodifiedPipeline [Command () c []])) (Sequential ())]

-- | An unmodified pipeline.
unmodifiedPipeline :: [BashSyn () Command] -> BashSyn () Pipeline
unmodifiedPipeline = Pipeline () False False False

-- | Parse a pipeline.
pipelineCommand :: Parser (BashSyn () Pipeline)
pipelineCommand = time
              <|> invert
              <|> pipeline1
              <?> "pipeline"
  where
    invert = do
        _ <- word "!"
        p <- pipeline0
        return $ modifyInverted not p

    time = do
        _ <- word "time"
        p <- posixFlag <|> invert <|> pipeline0
        return $ modifyTime (const True) p

    posixFlag = do
        _ <- word "-p"
        _ <- optional (word "--")
        p <- invert <|> pipeline0
        return $ modifyPosixFlag (const True) p

    pipeline0 = unmodifiedPipeline <$> commandList0
    pipeline1 = unmodifiedPipeline <$> commandList1

    commandList0 = option [] commandList1
    commandList1 = do
        c <- command
        pipelineSep c <|> pure [c]

    pipelineSep c = do
        c' <- c          <$ operator "|"
          <|> addRedir c <$ operator "|&"
        (c' :) <$> commandList0

    addRedir :: BashSyn () Command -> BashSyn () Command
    addRedir (Command ann c rs) = Command ann c (stderrRedir : rs)

    stderrRedir = Redir () (Just (IONumber () 2)) (OutAnd ()) (stringToWord "1")

    modifyInverted :: (Bool -> Bool) -> BashSyn () Pipeline -> BashSyn () Pipeline
    modifyInverted f (Pipeline ann timed timedPosix inverted cmds) =
        Pipeline ann timed timedPosix (f inverted) cmds
    modifyTime :: (Bool -> Bool) -> BashSyn () Pipeline -> BashSyn () Pipeline
    modifyTime f (Pipeline ann timed timedPosix inverted cmds) =
        Pipeline ann (f timed) timedPosix inverted cmds
    modifyPosixFlag :: (Bool -> Bool) -> BashSyn () Pipeline -> BashSyn () Pipeline
    modifyPosixFlag f (Pipeline ann timed timedPosix inverted cmds) =
        Pipeline ann timed (f timedPosix) inverted cmds

-- | Parse a compound list of commands.
compoundList :: Parser (BashSyn () List)
compoundList = List () <$ newlineList <*> many1 statement <?> "list"
  where
    statement = Statement () <$> andOr <*> option (Sequential ()) listTerm

    andOr = do
        p <- pipelineCommand
        let rest = And () p <$ operator "&&" <* newlineList <*> andOr
               <|> Or ()  p <$ operator "||" <* newlineList <*> andOr
        rest <|> pure (Last () p)

-- | Parse a possible empty compound list of commands.
inputList :: Parser (BashSyn () List)
inputList = newlineList *> option (List () []) compoundList

-- | Parse a command group, wrapped either in braces or in a @do...done@ block.
doGroup :: Parser (BashSyn () List)
doGroup = word "do" *> compoundList <* word "done"
      <|> word "{"  *> compoundList <* word "}"

-------------------------------------------------------------------------------
-- Compound commands
-------------------------------------------------------------------------------

-- | Parse a compound command.
shellCommand :: Parser (BashSyn () ShellCommand)
shellCommand = group
           <|> ifCommand
           <|> caseCommand
           <|> forCommand
           <|> whileCommand
           <|> untilCommand
           <|> selectCommand
           <|> condCommand
           <|> arithCommand
           <|> subshell
           <?> "compound command"

-- | Parse a @case@ command.
caseCommand :: Parser (BashSyn () ShellCommand)
caseCommand = Case () <$ word "case"
          <*> anyWord <* newlineList
          <*  word "in" <* newlineList
          <*> clauses
  where
    clauses = [] <$ word "esac"
          <|> do p <- pattern
                 c <- inputList
                 nextClause (CaseClause () p c)

    nextClause f = (:) <$> (f <$> clauseTerm) <* newlineList <*> clauses
               <|> [f (Break ())] <$ newlineList <* word "esac"

    pattern = optional (operator "(")
           *> anyWord `sepBy` operator "|"
          <*  operator ")"
          <?> "pattern list"

    clauseTerm = selectOperator operator <?> "case clause terminator"

-- | Parse a @while@ command.
whileCommand :: Parser (BashSyn () ShellCommand)
whileCommand = While () <$ word "while"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse an @until@ command.
untilCommand :: Parser (BashSyn () ShellCommand)
untilCommand = Until () <$ word "until"
           <*> compoundList
           <*  word "do" <*> compoundList <* word "done"

-- | Parse a list of words for a @for@ or @select@ command.
wordList :: Parser (BashSyn () WordList)
wordList = Args () <$ operator ";" <* newlineList
       <|> newlineList *> inList
       <?> "word list"
  where
    inList = WordList () <$ word "in" <*> many anyWord <* listTerm
         <|> pure (Args ())

-- | Parse a @for@ command.
forCommand :: Parser (BashSyn () ShellCommand)
forCommand = word "for" *> (arithFor_ <|> for_)
  where
    arithFor_ = ArithFor () <$> arith <* optional listTerm <*> doGroup

    for_ = For () <$> name <*> wordList <*> doGroup

-- | Parse a @select@ command.
selectCommand :: Parser (BashSyn () ShellCommand)
selectCommand = Select () <$ word "select" <*> name <*> wordList <*> doGroup

-- | Parse an @if@ command.
ifCommand :: Parser (BashSyn () ShellCommand)
ifCommand = word "if" *> if_
  where
    if_ = If () <$> compoundList <* word "then" <*> compoundList <*> alternative

    alternative = Just . singleton <$ word "elif" <*> if_
              <|> Just             <$ word "else" <*> compoundList <* word "fi"
              <|> Nothing          <$ word "fi"

-- | Parse a subshell command.
subshell :: Parser (BashSyn () ShellCommand)
subshell = Subshell () <$ operator "(" <*> compoundList <* operator ")"

-- | Parse a command group.
group :: Parser (BashSyn () ShellCommand)
group = Group () <$ word "{" <*> compoundList <* word "}"

-- | Parse an arithmetic command.
arithCommand :: Parser (BashSyn () ShellCommand)
arithCommand = Arith () <$> arith

-- | Parse a conditional command.
condCommand :: Parser (BashSyn () ShellCommand)
condCommand = Cond () <$ word "[[" <*> expr <* word "]]"
  where
    expr = buildExpressionParser opTable term

    term = operator "(" *> expr <* operator ")"
       <|> Cond.Unary <$> unaryOp <*> condWord
       <|> (condWord >>= wordTerm)

    wordTerm w = Cond.Binary w Cond.StrMatch <$ try (word "=~") <*> regexWord
             <|> Cond.Binary w <$> binaryOp <*> condWord
             <|> pure (Cond.Unary Cond.NonzeroString w)

    opTable =
        [ [Prefix (Cond.Not <$ word "!")]
        , [Infix  (Cond.And <$ operator "&&") AssocLeft]
        , [Infix  (Cond.Or  <$ operator "||") AssocLeft]
        ]

    condWord = anyWord `satisfying` (/= stringToWord "]]")
           <|> stringToWord <$> anyOperator
           <?> "word"

    condOperator op = condWord `satisfying` (== stringToWord op) <?> op

    unaryOp  = selectOperator condOperator <?> "unary operator"
    binaryOp = selectOperator condOperator <?> "binary operator"

    regexWord = stringToWord . concat <$> many1 (regexPart " \t\r\n") <* skipSpace
            <?> "regular expression"

    regexPart delims = regexParens
                   <|> regexSingleQuote
                   <|> regexDoubleQuote
                   <|> regexEscape
                   <|> regexChar delims

    regexDelimiters :: Char -> Parser String -> Char -> Parser String
    regexDelimiters begin middle end = do
        _ <- char begin
        parts <- many middle
        _ <- char end
        return $ [begin] ++ concat parts ++ [end]

    regexParens = regexDelimiters '(' (regexPart ")") ')'

    regexSingleQuote = regexDelimiters '\'' singleQuoteChar '\''
      where
        singleQuoteChar = sequence [char '\\', anyChar]
                      <|> (:[]) <$> noneOf "'"

    regexDoubleQuote = regexDelimiters '"' doubleQuoteChar '"'
      where
        doubleQuoteChar = sequence [char '\\', anyChar]
                      <|> (:[]) <$> noneOf "\""

    regexEscape = sequence [char '\\', anyChar]

    regexChar :: [Char] -> Parser String
    regexChar delims = (:[]) <$> noneOf delims

-------------------------------------------------------------------------------
-- Coprocesses
-------------------------------------------------------------------------------

-- | Parse a coprocess command.
coproc :: Parser (BashSyn () ShellCommand)
coproc = word "coproc" *> coprocCommand <?> "coprocess"
  where
    coprocCommand = Coproc () <$> option "COPROC" name
                           <*> (Command () <$> shellCommand <*> pure [])
                </> Coproc () "COPROC" <$> simpleCommand

-------------------------------------------------------------------------------
-- Function definitions
-------------------------------------------------------------------------------

-- | Parse a function definition.
functionDef :: Parser (BashSyn () ShellCommand)
functionDef = functionDef2
          <|> functionDef1
          <?> "function definition"
  where
    functionDef1 = FunctionDef ()
               <$> try (word "function" *> functionName
                        <* optional functionParens <* newlineList)
               <*> functionBody

    functionDef2 = FunctionDef ()
               <$> try (functionName <* functionParens <* newlineList)
               <*> functionBody

    functionParens = operator "(" <* operator ")"

    functionBody = unwrap <$> group
               <|> singleton <$> shellCommand

    unwrap :: BashSyn () ShellCommand -> BashSyn () List
    unwrap (Group () l) = l
    unwrap _            = List () []

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

-- | Parse a single command.
command :: Parser (BashSyn () Command)
command = Command () <$> compoundCommand <*> redirList
      <|> simpleCommand
      <?> "command"
  where
    compoundCommand = shellCommand
                  <|> coproc
                  <|> functionDef

-- | Parse an entire script (e.g. a file) as a list of commands.
script :: Parser (BashSyn () List)
script = skipSpace *> inputList <* eof
