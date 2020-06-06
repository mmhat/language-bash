{
module Language.Bash.Parse.Alex where
}

%wrapper "basic"

:-
    "if"        { \_ -> TokenIf }
    "then"      { \_ -> TokenThen }
    "else"      { \_ -> TokenElse }
    "elif"      { \_ -> TokenElif }
    "fi"        { \_ -> TokenFi }
    "case"      { \_ -> TokenCase }
    "esac"      { \_ -> TokenEsac }
    "for"       { \_ -> TokenFor }
    "select"    { \_ -> TokenSelect }
    "while"     { \_ -> TokenWhile }
    "until"     { \_ -> TokenUntil }
    "do"        { \_ -> TokenDo }
    "done"      { \_ -> TokenDone }
    "in"        { \_ -> TokenIn }
    "function"  { \_ -> TokenFunction }
    "time"      { \_ -> TokenTime }
    "{"         { \_ -> TokenBraceOpen }
    "}"         { \_ -> TokenBraceClose }
    "!"         { \_ -> TokenBang }
    "[["        { \_ -> TokenCondStart }
    "]]"        { \_ -> TokenCondEnd }
    "coproc"    { \_ -> TokenCoproc }

    "--"        { \_ -> TokenTimeIgn }
    "-p"        { \_ -> TokenTimeOpt }
    "&&"        { \_ -> TokenAndAnd }
    "||"        { \_ -> TokenOrOr }
    ">>"        { \_ -> TokenGreaterGreater }
    "<<"        { \_ -> TokenLessLess }
    "<&"        { \_ -> TokenLessAnd }
    ">&"        { \_ -> TokenGreaterAnd }
    ";;"        { \_ -> TokenSemiSemi }
    ";&"        { \_ -> TokenSemiAnd }
    ";;&"       { \_ -> TokenSemiSemiAnd }
    "<<-"       { \_ -> TokenLessLessMinus }
    "<<<"       { \_ -> TokenLessLessLess }
    "&>"        { \_ -> TokenAndGreater }
    "&>>"       { \_ -> TokenAndGreaterGreater }
    "<>"        { \_ -> TokenLessGreater }
    ">|"        { \_ -> TokenGreaterBar }
    "|&"        { \_ -> TokenBarAnd }

    ">"         { \_ -> TokenGreater }
    "<"         { \_ -> TokenLess }
    "-"         { \_ -> TokenMinus }
    ";"         { \_ -> TokenSemi }
    "("         { \_ -> TokenParOpen }
    ")"         { \_ -> TokenParClose }
    "|"         { \_ -> TokenBar }
    "&"         { \_ -> TokenAnd }
    \ +         {       TokenWhiteSpaces }
    \\\n        { \_ -> TokenEscapedChar '\n' }  -- Included by us
    \n          { \_ -> TokenNewline }

    -- Included by us
    \'          { \_ -> TokenSingleQuote }
    \"          { \_ -> TokenDoubleQuote }
    \\.         { TokenEscapedChar . head . tail }
    .           { TokenChar . head }
{

data Token
    = TokenIf
    | TokenThen
    | TokenElse
    | TokenElif
    | TokenFi
    | TokenCase
    | TokenEsac
    | TokenFor
    | TokenSelect
    | TokenWhile
    | TokenUntil
    | TokenDo
    | TokenDone
    | TokenIn
    | TokenFunction
    | TokenTime
    | TokenBraceOpen
    | TokenBraceClose
    | TokenBang
    | TokenCondStart
    | TokenCondEnd
    | TokenCoproc

    | TokenTimeIgn
    | TokenTimeOpt
    | TokenAndAnd
    | TokenOrOr
    | TokenGreaterGreater
    | TokenLessLess
    | TokenLessAnd
    | TokenGreaterAnd
    | TokenSemiSemi
    | TokenSemiAnd
    | TokenSemiSemiAnd
    | TokenLessLessMinus
    | TokenLessLessLess
    | TokenAndGreater
    | TokenAndGreaterGreater
    | TokenLessGreater
    | TokenGreaterBar
    | TokenBarAnd
    | TokenEOF

    | TokenGreater
    | TokenLess
    | TokenMinus
    | TokenSemi
    | TokenParOpen
    | TokenParClose
    | TokenBar
    | TokenAnd
    | TokenWhiteSpaces String
    | TokenNewline

    | TokenSingleQuote
    | TokenDoubleQuote
    | TokenEscapedChar Char
    | TokenChar Char

    deriving Show
}
