{-# LANGUAGE OverloadedStrings #-}
module Presilk.Parse
    ( Presilk.Parse.parse, parse1
    )
    where
--------------------------------------------------------------------------------
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer       qualified as L
import Data.Text                        qualified as T
import Data.Text                        (Text)
import Data.Void
import Data.Char
import Control.Applicative              hiding (many, some)
import Control.Monad

import Presilk.Syntax
--------------------------------------------------------------------------------

type P = Parsec Void Text

sexp :: P Sexp
sexp = form
   <|> (Atom <$> atom)
   <|> quote
   <|> quasiquote
   <|> unquote

mkQuote :: (Sexp -> Sexp) -> P a -> P Sexp
mkQuote f q = f <$> (q *> sexp)

-- potentially a bug?
-- >>> parse1 ",,doge"
-- Right (List [Atom (Symbol "unquote"),List [Atom (Symbol "unquote"),Atom (Symbol "doge")]])

-- we don't use `lexeme` or `special` here to disallow whitespace between the
-- quote mark and the quoted form.

quote, quasiquote, unquote :: P Sexp
quote = mkQuote (sexpApply "quote") "'"
quasiquote = mkQuote (sexpApply "quasiquote") "`"
unquote = mkQuote (sexpApply "unquote") ","

form :: P Sexp
form = list <|> vec
  where
    vec = fmap (List . mkvec) $ listWith (special "[") (special "]") sexp
    mkvec = (Atom (Symbol "vector") :)

list :: P Sexp
list = fmap List $ listWith (special "(") (special ")") sexp

atom :: P Atom
atom = (Symbol <$> symbol)
   <|> (LitInt <$> litint)
   <|> (LitStr <$> litstr)

symbol :: P Symbol
symbol = lexeme (T.cons <$> satisfy isHeadChar <*> takeWhileP Nothing isSymChar)
  where
    isHeadChar c = isSymChar c && (c /= '\'') && not (isDigit c)
    isSymChar c = (c `notElem` ['(',')','[',']',',','`','"'])
               && (not . isSpace $ c)

litint :: P Int
litint = lexeme $ L.signed empty L.decimal

litstr :: P Text
litstr = lexeme . fmap T.pack $ str
    where str = char '"' *> manyTill L.charLiteral (char '"')

--------------------------------------------------------------------------------

listWith :: P l        -- ^ open list
         -> P r        -- ^ close list
         -> P a
         -> P [a]
listWith l r a = l *> many a <* r

parse1 :: Text -> Either _ Sexp
parse1 = runParser (sc *> sexp <* eof) "Presilk.Parse.parse1"

parse :: Text -> Either _ [Sexp]
parse = runParser (many sexp <* eof) "Presilk.Parse.parse"

sc :: P ()
sc = void . many $ void (satisfy isSpace) <|> comment
  where
    comment = L.skipLineComment ";"
          <|> L.skipBlockCommentNested "#|" "|#"
          <|> void (special "#;" *> sexp)

lexeme :: P a -> P a
lexeme = L.lexeme sc

special :: Text -> P Text
special = L.symbol sc

