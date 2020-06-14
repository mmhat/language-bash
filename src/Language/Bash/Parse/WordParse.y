{
module Language.Bash.Parse.WordParse where

import Language.Bash.Parse.WordLex

}

%name wordParse
%error { parseError }
-- %lexer { alexScanTokens }

%tokentype { Token }

%token
    COMMENT     { TokenComment     }
    ESC         { TokenEscape      }
    DOLLAR      { TokenDollar      }
    DQUOTE      { TokenDouble      }
    META        { TokenMetaChar $$ }
    SQUOTE      { TokenSingle      }
    WORD        { TokenWordChar $$ }

%%

word: sword     { $1 }

sword: SQUOTE schars SQUOTE      { WordSingle $2 }

schars: {-# empty #-}   { [] }
      | schars schar    { $2 : $1 }

schar: COMMENT  { WordChar '#' }
     | ESC      { WordChar '\\' }
     | DOLLAR   { WordChar '$' }
     | DQUOTE   { WordChar '"' }
     | META     { WordChar $1 }
     | WORD     { WordChar $1 }

{

data Foo
    = WordSingle [Foo]
    | WordChar Char

    deriving Show

parseError :: [Token] -> a
parseError = error . show
}
