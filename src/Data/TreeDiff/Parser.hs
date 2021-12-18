{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities to parse 'Expr'.
--
-- /Note:/ we don't parse diffs.
module Data.TreeDiff.Parser (
    exprParser
    ) where

import Control.Applicative (many, optional, (<|>))
import Data.Char           (chr, isAlphaNum, isPunctuation, isSymbol)
import Prelude ()
import Prelude.Compat

import Text.Parser.Char            (CharParsing (anyChar, char, satisfy))
import Text.Parser.Combinators     (between, (<?>))
import Text.Parser.Token
       (TokenParsing (highlight, token), braces, brackets, commaSep,
       hexadecimal, parens, symbolic)
import Text.Parser.Token.Highlight
       (Highlight (Identifier, StringLiteral, Symbol))

import Data.TreeDiff.Expr

import qualified Data.TreeDiff.OMap as OMap

-- | Parsers for 'Expr' using @parsers@ type-classes.
--
-- You can use this with your parser-combinator library of choice:
-- @parsec@, @attoparsec@, @trifecta@...
exprParser :: (Monad m, TokenParsing m) => m Expr
exprParser = apprecP <|> lstP

lstP :: forall m. (Monad m, TokenParsing m) => m Expr
lstP = Lst <$> brackets (commaSep exprParser)
    <?> "list"

apprecP :: forall m. (Monad m, TokenParsing m) => m Expr
apprecP = do
    r <- recP
    case r of
        Right e -> return e
        Left n  -> App n <$> many litP'

fieldP :: forall m. (Monad m, TokenParsing m) => m (FieldName, Expr)
fieldP = (,) <$> litP <* symbolic '=' <*> exprParser

litP :: forall m. (Monad m, TokenParsing m) => m String
litP = atomP <|> identP <|> stringP

recP :: forall m. (Monad m, TokenParsing m) => m (Either String Expr)
recP = mk <$> litP <*> optional (braces (commaSep fieldP)) where
    mk n Nothing   = Left n
    mk n (Just fs) = Right (Rec n (OMap.fromList fs))

litP' :: forall m. (Monad m, TokenParsing m) => m Expr
litP' = mk <$> recP <|> parens exprParser <|> lstP
  where
    mk (Left n)  = App n []
    mk (Right e) = e

identP :: forall m. (Monad m, TokenParsing m) => m String
identP = token (highlight Identifier lit) where
    lit :: m [Char]
    lit = (:) <$> firstLetter <*> many restLetter
        <?> "identifier"

    firstLetter :: m Char
    firstLetter = satisfy (\c -> valid' c && c /= '-' && c /= '+')

    restLetter :: m Char
    restLetter = satisfy valid'

stringP :: forall m. (Monad m, TokenParsing m) => m String
stringP = token (highlight StringLiteral lit) where
    lit :: m [Char]
    lit = mk <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
        <?> "atom"

    mk :: [[Char]] -> String
    mk ss = "\"" ++ concat ss ++ "\""

    stringChar :: m [Char]
    stringChar = stringLetter <|> stringEscape
        <?> "string character"

    stringEscape :: m [Char]
    stringEscape = (\x y -> [x,y]) <$> char '\\' <*> anyChar

    stringLetter :: m [Char]
    stringLetter = return <$> satisfy (\c -> c /= '\\' && c /= '"')

atomP :: forall m. (Monad m, TokenParsing m) => m String
atomP = token (highlight Symbol lit) where
    lit :: m [Char]
    lit = between (char '`') (char '`' <?> "end of atom") (many atomChar)
        <?> "atom"

    atomChar :: m Char
    atomChar = atomLetter <|> atomEscape <|> char ' '
        <?> "atom character"

    atomEscape :: m Char
    atomEscape = char '\\' *> (char '\\' <|> char '`' <|> escapedHex)

    escapedHex :: m Char
    escapedHex = chr . fromInteger <$> hexadecimal <* char ';'

    atomLetter :: m Char
    atomLetter = satisfy (\c -> c /= '\\' && c /=  '`' && valid c)

valid :: Char -> Bool
valid c = isAlphaNum c || isSymbol c || isPunctuation c

valid' :: Char -> Bool
valid' c = valid c && c `notElem` "[](){}`\","
