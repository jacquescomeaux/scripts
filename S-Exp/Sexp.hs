#!/usr/bin/env stack
-- stack --resolver lts-19.6 script
{-# LANGUAGE LambdaCase #-}
import Control.Applicative (Alternative (..))
import Data.Foldable (asum)
import Data.List (intercalate, union, intersect)
import System.Environment (getArgs)

data SexpNode
    = SexpString String
    | SexpList [SexpNode]
  deriving (Eq, Show)

data Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s ->
        case p s of
            Just (x, s') -> Just (f x, s')
            Nothing      -> Nothing

instance Applicative Parser where
    (<*>) (Parser pf) ~(Parser px) = Parser $ \s -> do
        (f, s0) <- pf s
        (x, s1) <- px s0
        Just (f x, s1)
    pure x = Parser $ \s -> Just (x, s)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (<|>) (Parser p1) (Parser p2) = Parser $ \s ->
        p1 s <|> p2 s

char :: Char -> Parser Char
char x = Parser $ \case
    (c : cs)
        | x == c -> Just (c, cs)
    _ -> Nothing

whites, specials :: [Char]
whites = [' ', '\t', '\n']
specials = ['"', '(', ')']

white :: Parser Char
white = asum (char <$> whites)

lparen, rparen, quote :: Parser Char
lparen = char '('
rparen = char ')'
quote  = char '"'

anyChar :: Parser Char
anyChar = Parser $ \case
    (c : cs)
        | elem c specials -> Nothing
        | elem c whites   -> Nothing
        | otherwise       -> Just (c, cs)
    [] -> Nothing

anyQuotedChar :: Parser Char
anyQuotedChar = Parser $ \case
    (c : cs)
        | c == '"'  -> Nothing
        | otherwise -> Just (c, cs)
    [] -> Nothing

quotedString :: Parser String
quotedString = quote *> many anyQuotedChar <* quote

sexp :: Parser SexpNode
sexp = list <|> atom
  where
    atom, list :: Parser SexpNode
    atom = SexpString <$> (quotedString <|> some anyChar)
    list = SexpList <$> (lparen *> many (sexp <* many white) <* rparen)

showSexp :: SexpNode -> String
showSexp (SexpString s)
    | null s || special s = "\"" ++ s ++ "\""
    | otherwise           = s
  where
    special :: String -> Bool
    special st = not . null $ intersect st (union whites specials)
showSexp (SexpList l) = "(" ++ intercalate " " (showSexp <$> l) ++ ")"

toSexp :: String -> Maybe SexpNode
toSexp line = case runParser sexp line of
    Just (s, "") -> Just s
    _            -> Nothing

-- | S-Exp catamorhism
sexpnode :: (String -> a) -> ([a] -> a) -> SexpNode -> a
sexpnode f _ (SexpString s) = f s
sexpnode f g (SexpList l)   = g (sexpnode f g <$> l)

-- | Remove metadata (CAst nodes)
deCAst :: SexpNode -> SexpNode
deCAst (SexpList [SexpList [SexpString "v", val], SexpList [SexpString "loc", _loc]]) = val
deCAst x = x

-- | Recursively apply an S-Exp transformation
recursively :: (SexpNode -> SexpNode) -> SexpNode -> SexpNode
recursively f = sexpnode (f . SexpString) (f . SexpList)

convertFile :: FilePath -> IO ()
convertFile input = do
    ls <- readLines input
    flip traverse (zip [0..] ls) $ \(i, l) -> do
        case toSexp l of
            Just sp -> putStrLn $ showSexp (recursively deCAst sp)
            Nothing -> putStrLn $ "Parsing failed on line " ++ show i ++ "."
    pure ()
  where
    readLines :: FilePath -> IO [String]
    readLines fp = lines <$> readFile fp

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Error: missing input file"
        else convertFile (head args)
