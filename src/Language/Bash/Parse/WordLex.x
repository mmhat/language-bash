{
module Language.Bash.Parse.WordLex where
}

%wrapper "basic"

$metachar = [ \| \& \; \( \) \< \> \  \t \n ]
$wordchar = . # $metachar

:-

    \$          { \_ -> TokenDollar    }
    \#          { \_ -> TokenComment   }
    \\          { \_ -> TokenEscape    }
    \'          { \_ -> TokenSingle    }
    \"          { \_ -> TokenDouble    }
    \n          { \_ -> TokenNewline   }
    $metachar   { TokenMetaChar . head }
    $wordchar   { TokenWordChar . head }

{

data Token
    = TokenComment
    | TokenDollar
    | TokenDouble
    | TokenEscape
    | TokenMetaChar Char
    | TokenNewline
    | TokenWordChar Char
    | TokenSingle

    deriving Show
}
