{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, PatternGuards, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
-- | Shell expansions.
module Language.Bash.Expand
    ( braceExpand
    , TildePrefix(..)
    , tildePrefix
    , splitWord
    ) where

import Prelude hiding (Word)

import Control.Applicative
import Control.Exception (displayException)
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Functor
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid            ((<>))
import qualified Text.Megaparsec as MP
import Text.Parsec.Combinator hiding (optional, manyTill)
import Text.Parsec.Prim       hiding ((<|>), many, token)
import Text.Parsec.String     ()
import Data.Text.Prettyprint.Doc (Pretty(..))

import Language.Bash.Pretty
import Language.Bash.Word     hiding (prefix)

import Debug.Trace

instance MP.Stream Word where
    type Token Word = Span
    type Tokens Word = Word

    tokensToChunk _ = id

    chunkToTokens _ = id

    chunkLength _ = length

    take1_ = L.uncons

    takeN_ i xs | i <= 0 = Just ([], xs)
    takeN_ _ [] = Nothing
    takeN_ i xs = Just $ L.splitAt i xs

    takeWhile_ p = L.break (not . p)

    showTokens _ = prettyText . NE.toList

    -- TODO: Tabs to spaces
    reachOffset o (MP.PosState {..}) = (spos, prettyText pstateInput, pos')
        where
            MP.SourcePos {..} = pstateSourcePos
            spos = pstateSourcePos
                { MP.sourceColumn = MP.mkPos $ MP.unPos sourceColumn + o
                }
            pos' = MP.PosState
                { pstateInput = post
                , pstateOffset = max pstateOffset o
                , pstateSourcePos = spos
                , pstateTabWidth = pstateTabWidth
                , pstateLinePrefix = undefined
                }
            (_, post) = L.splitAt (o - pstateOffset) pstateInput

instance MP.ShowErrorComponent () where
    showErrorComponent = show

-- | A parser over words.
type Parser = Parsec Word ()

infixl 3 </>

-- | Backtracking choice.
(</>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p </> q = try p <|> q

-- | Run a 'Parser', failing on a parse error.
parseUnsafe :: String -> Parser a -> Word -> a
parseUnsafe f p w = case parse p (prettyText w) w of
    Left  e -> error $ "Language.Bash.Expand." ++ f ++ ": " ++ show e
    Right a -> a

-- | Parse a general token.
token :: (Span -> Maybe a) -> Parser a
token = tokenPrim (const "") (\pos _ _ -> pos)

-- | Parse an unquoted character satisfying a predicate.
satisfy :: (Char -> Bool) -> Parser Span
satisfy p = token $ \t -> case t of
    Char c | p c -> Just t
    _            -> Nothing

-- | Parse an unquoted character satisfying a predicate.
satisfy' :: (Char -> Bool) -> Parser Char
satisfy' p = token $ \t -> case t of
    Char c | p c -> Just c
    _            -> Nothing

-- | Parse a span that is not an unquoted character satisfying a predicate.
except :: (Char -> Bool) -> Parser Span
except p = token $ \t -> case t of
    Char c | p c -> Nothing
    _            -> Just t

-- | Parse an unquoted character.
char :: Char -> Parser Span
char c = token $ \t -> case t of
    Char d | c == d -> Just t
    _               -> Nothing

-- | Parse an unquoted string.
string :: String -> Parser Word
string = traverse char

-- | Parse one of the given characters.
oneOf :: [Char] -> Parser Span
oneOf cs = satisfy (`elem` cs)

-- | Parse anything but a quoted character.
noneOf :: [Char] -> Parser Span
noneOf cs = except (`elem` cs)

-- | Read a number.
readNumber :: MonadPlus m => String -> m Int
readNumber s = case reads (dropPlus s) of
    [(n, "")] -> return n
    _         -> mzero
  where
    dropPlus ('+':t) = t
    dropPlus t       = t

-- | Read a letter.
readAlpha :: MonadPlus m => String -> m Char
readAlpha [c] | isAlpha c = return c
readAlpha _               = mzero

-- | Create a list from a start value, an end value, and an increment.
enum :: (Ord a, Enum a) => a -> a -> Maybe Int -> [a]
enum x y inc = map toEnum [fromEnum x, fromEnum x + step .. fromEnum y]
  where
    step = case inc of
        Nothing | y > x     -> 1
                | otherwise -> 1
        Just i              -> i
{-
data BraceExpansion
    = Token Span
    | Expansion [BraceExpansion]
    | Unexpandable [BraceExpansion]
    deriving Eq

foo = untilUnchanged go . map Token
    where
        go xs = let
            (pre, ys) = findLastOpenBrace xs
            Just (x, zs) = tryAmbleExpansion ys <|> pure (Unexpandable ys, mempty)  -- TODO: trySequenceExpansion
            in pre ++ [x] ++ zs

        findLastOpenBrace = L.foldl' f ([], [])
            where
                f (pre, xs@(Token (Char '{'):_)) x = (x:pre, xs)
                f (pre, xs                     ) x = (pre, x:xs)

        tryAmbleExpansion (Token (Char '{'):xs) = do
            (ys, zs) <- break' '}' xs
            ys' <- split' ',' ys
            return (_f ys, zs)

untilUnchanged :: Eq a => (a -> a) -> a -> a
untilUnchanged f x
    | f x == x = x
    | otherwise = untilUnchanged f (f x)

break' c xs = case break (== Token (Char c)) xs of
    (pre, (Token (Char c)):post) -> Just (pre, post)
    _ -> Nothing

split' c xs
    | Token (Char c) `notElem` xs = Nothing
    | otherwise = 
-}

braceExpand :: Word -> [Word]
braceExpand xs = case MP.runParser (braceExpandP "") "" xs of
    Left e    -> error $ "Language.Bash.Expand.braceExpand: " ++ displayException e
    Right res -> res

braceExpandP :: String -> MP.ParsecT () Word m [Word]
braceExpandP stop = do
    inpt <- MP.getInput
    traceShowM ("braceExpandP", stop, prettyText inpt)
    preamble <- MP.takeWhileP Nothing notBraceOpenOrStop
    let preamble' = map unescape preamble
    traceShowM ("preamble", prettyText preamble')
    x <- expandBrace <|> noExpand
    traceShowM ("x", map prettyText x)
    postamble <- doStop <|> braceExpandP stop
    traceShowM ("postamble", map prettyText postamble)
    let res = L.delete [] $ [ xx ++ yy | xx <- map (preamble' ++) x, yy <- postamble ]
    traceShowM ("result", map prettyText res)
    return $ [ xx ++ yy | xx <- map (preamble' ++) x, yy <- postamble ]
    where
        expandBrace = MP.try $ do
            _ <- MP.single $ Char '{'
            x <- MP.try expandAmble <|> expandSequence
            _ <- MP.single $ Char '}'
            return x

        expandAmble = do
            inpt <- MP.getInput
            traceShowM ("expandAmble", prettyText inpt)
            x <- braceExpandP ","
            xs <- some $ do
                _ <- MP.single $ Char ','
                braceExpandP ",}"
            return $ concat $ x : xs

        expandSequence = do
            inpt <- MP.getInput
            traceShowM ("expandSequence", prettyText inpt)
            (isPadded1, l1, s) <- int
            _ <- MP.chunk [Char '.', Char '.']
            (isPadded2, l2, e) <- int
            inc <- MP.option 1 $ do
                _ <- MP.chunk [Char '.', Char '.']
                (_, _, inc) <- int
                return inc
            let inc'     = if s < e then s + abs inc else s - abs inc
                xs       = [s,inc' .. e]
                isPadded = isPadded1 || isPadded2
                l        = maximum [l1, l2, length $ show $ head xs, length $ show $ last xs]
            return $ map (intToWord isPadded l) xs

        noExpand = do
            x  <- MP.option [] $ fmap (\x -> [x]) $ MP.single (Char '{')
            xs <- many $ MP.satisfy notBraceCloseOrStop
            return [x ++ xs]

        doStop = do
            MP.eof <|> void (MP.lookAhead $ MP.satisfy isStop)
            return [[]]

        unescape (Escape c) = Char c
        unescape x = x

        isStop (Char c) = c `elem` stop
        isStop _ = False

        notBraceOpenOrStop (Char '{') = False
        notBraceOpenOrStop (Char c  ) | c `elem` stop = False
        notBraceOpenOrStop _          = True

        notBraceCloseOrStop (Char '}') = False
        notBraceCloseOrStop (Char c  ) | c `elem` stop = False
        notBraceCloseOrStop _          = True

intToWord :: Bool -> Int -> Int -> Word
intToWord False _ i = stringToWord $ show i
intToWord _ l i = let
    sign = if i < 0 then "-" else "" 
    l'   = length $ show i
    pad  = replicate (l - l') '0'
    in stringToWord $ sign ++ pad ++ show (abs i)


int :: MP.ParsecT () Word m (Bool, Int, Int)
int = do
    msign <- MP.optional $ (MP.single (Char '+') $> id) <|> (MP.single (Char '-') $> negate)
    xs <- some $ MP.oneOf $ stringToWord ['0'..'9']
    let sign     = fromMaybe id msign
        xs'      = map (\(Char c) -> c) xs
        l        = maybe 0 (const 1) msign + length xs
        isPadded = head xs' == '0'
    return (isPadded, l, sign $ read xs')

-- | Brace expand a word.
braceExpand2 :: Word -> [Word]
braceExpand2 = parseUnsafe "braceExpand" start
  where
    prefix a bs = map (a ++) bs
    cross as bs = [a ++ b | a <- as, b <- bs]

    -- A beginning empty brace is ignored.
    start = prefix <$> string "{}" <*> expr ""
        </> expr ""

    expr delims = foldr ($) [[]] <$> many (exprPart delims)

    exprPart delims = cross <$ char '{' <*> brace delims <* char '}'
                  </> prefix <$> emptyBrace
                  </> prefix . (:[]) <$> noneOf delims

    brace delims = concat <$> braceParts delims
               </> sequenceExpand
               </> map (\s -> stringToWord "{" ++ s ++ stringToWord "}") <$> expr ",}"

    -- The first part of the outermost brace expression is not delimited by
    -- a close brace.
    braceParts delims =
        (:) <$> expr (if ',' `elem` delims then ",}" else ",") <* char ','
            <*> expr ",}" `sepBy1` char ','

    emptyBrace = do
        a <- token $ \t -> case t of
            Char c   | c `elem` ws -> Just t
            Escape c | c `elem` ws -> Just t
            _                      -> Nothing
        b <- char '{'
        c <- char '}' <|> oneOf ws
        return [a, b, c]
      where
        ws = " \t\r\n"

    sequenceExpand = do
        a   <- sequencePart
        b   <- string ".." *> sequencePart
        c   <- optional (string ".." *> sequencePart)
        inc <- traverse readNumber c
        map stringToWord <$> (numExpand a b inc <|> charExpand a b inc)
      where
        sequencePart = many1 (satisfy' isAlphaNum)

    charExpand a b inc = do
        x <- readAlpha a
        y <- readAlpha b
        return . map (:[]) $ enum x y inc

    numExpand a b inc = do
        x <- readNumber a
        y <- readNumber b
        return . map showPadded $ enum x y inc
      where
        width = max (length a) (length b)

        isPadded ('-':'0':_:_) = True
        isPadded ('0':_:_)     = True
        isPadded _             = False

        showPadded = if isPadded a || isPadded b then pad width else show

        pad w n
            | n < 0     = '-' : pad (w - 1) (negate n)
            | otherwise = replicate (w - length s) '0' ++ s
          where
            s = show n

-- | A tilde prefix.
data TildePrefix
    = Home              -- ^ @~/foo@
    | UserHome String   -- ^ @~fred/foo@
    | PWD               -- ^ @~+/foo@
    | OldPWD            -- ^ @~-/foo@
    | Dirs Int          -- ^ @~N@, @~+N@, @~-N@
    deriving (Eq, Read, Show)

instance Pretty TildePrefix where
    pretty Home         = "~"
    pretty (UserHome s) = "~" <> pretty s
    pretty PWD          = "~+"
    pretty OldPWD       = "~-"
    pretty (Dirs n)     = "~" <> pretty n

-- | Strip the tilde prefix of a word, if any.
tildePrefix :: Word -> Maybe (TildePrefix, Word)
tildePrefix w = case parseUnsafe "tildePrefix" split w of
    ('~':s, w') -> Just (readPrefix s, w')
    _           -> Nothing
  where
    split = (,) <$> many (satisfy' (/= '/')) <*> getInput

    readPrefix s
        | s == ""                = Home
        | s == "+"               = PWD
        | s == "-"               = OldPWD
        | Just n <- readNumber s = Dirs n
        | otherwise              = UserHome s

-- | Split a word on delimiters.
splitWord :: [Char] -> Word -> [Word]
splitWord ifs = parseUnsafe "splitWord" $ ifsep *> many (word <* ifsep)
  where
    ifsep = many  (oneOf  ifs)
    word  = many1 (noneOf ifs)
