{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Utilities to parse 'Expr'.
--
-- /Note:/ we don't parse diffs.
module Data.TreeDiff.Parser (
    exprParser,
    showToExpr,
    ShowParseFailed,
    unsafeShowToExpr,
    ) where

import Control.Applicative (many, optional, (<|>))
import Control.Exception   (Exception(..))
import Data.Char           (chr, isAlphaNum, isPunctuation, isSymbol)

import GHC.Stack (HasCallStack)
import Text.Parser.Char            (CharParsing (anyChar, char, satisfy))
import Text.Parser.Combinators     (between, (<?>))
import Text.Parser.Token
       (TokenParsing (highlight, token), braces, brackets, commaSep,
       hexadecimal, parens, symbolic)
import Text.Parser.Token.Highlight
       (Highlight (Identifier, StringLiteral, Symbol))

import Data.TreeDiff.Expr
import Text.Parsec (ParseError)

import qualified Control.Exception as Exception
import qualified Data.TreeDiff.OMap as OMap
import qualified Text.Parsec as Parsec

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

{-| Parse an `Expr` from a type's `Show` instance.  This can come in handy if a
    type already has a `Show` instance and you don't want to have to derive
    `ToExpr` for that type and all of its dependencies.
-}
showToExpr :: Show a => a -> Either ShowParseFailed Expr
showToExpr a =
    case Parsec.parse exprParser "" (show a) of
        Left exception -> Left ShowParseFailed{ exception }
        Right expr     -> Right expr

instance Exception ShowParseFailed where
    displayException ShowParseFailed{ exception } =
        "Failed to parse an Expr from the output of show\n\
        \\n\
        \This might be due to the Show instance (or one of the Show instances it depends\n\
        \on) not being derived.\n\
        \\n\
        \Parsing error:\n\
        \\n\
        \" <> show exception

-- | `unsafeShowToExpr` failed to parse the output from `show` into an `Expr`
--
-- This usually means that the type (or one of its dependencies) has a `Show`
-- instance that was not derived.
newtype ShowParseFailed = ShowParseFailed{ exception :: ParseError }
    deriving (Show)

{-| You can use this to implement the `toExpr` method of the `ToExpr` class.
    However, this is a partial function that is only safe to use for derived
    `Show` instances and might fail for other types of instances.

    If this function fails it will `Exception.throw` a `ShowParseFailed`
    exception.
-}
unsafeShowToExpr :: HasCallStack => Show a => a -> Expr
unsafeShowToExpr a =
    case showToExpr a of
        Left  exception -> Exception.throw exception
        Right expr      -> expr
