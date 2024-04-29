{-# LANGUAGE OverloadedStrings #-}
module Presilk.Parse
    ( parse, parse1, pprint
    )
    where
--------------------------------------------------------------------------------
import Data.Text                        qualified as T
import Data.Text                        (Text)
import Data.Void
import Data.Char
import Data.Function
import Data.Functor
import Control.Applicative              hiding ((<|>), many, some)
import Control.Monad

import Data.SCargot
import Data.SCargot.Repr
import Data.SCargot.Repr.WellFormed
import Data.SCargot.Atom
import Data.SCargot.Common
import Data.SCargot.Comments

import Text.Parsec                      hiding (parse)
import Text.Parsec.Text                 (Parser)

import Presilk.Syntax
--------------------------------------------------------------------------------

type P = Parsec Void Text

ident :: Parser Text
ident = T.pack <$> ((:) <$> initial <*> many rest)
    where
        isParen = (`elem` ("[({})]" :: String))
        isOk c = not $ isSpace c || isControl c || isParen c
        initial = satisfy \c ->
            isOk c && not (isDigit c)
        rest = satisfy isOk

vecReader :: Parser (SExpr Atom) -> Parser (SExpr Atom)
vecReader p = SCons (SAtom (Symbol "vector")) <$> es
  where
    es = (char ']' $> SNil)
     <|> (SCons <$> p <*> es)

mkQuoter :: Symbol -> Parser (SExpr Atom) -> Parser (SExpr Atom)
mkQuoter q p = quote <$> p
  where
    quote e = SCons (SAtom (Symbol q)) (SCons e SNil)

--------------------------------------------------------------------------------

marshal :: WellFormedSExpr Atom -> Sexp
marshal (L es) = List $ marshal <$> es
marshal (A a) = Atom a

marshalBack :: Sexp -> WellFormedSExpr Atom
marshalBack (List es) = L $ marshalBack <$> es
marshalBack (Atom a) = A a

printAtom :: Atom -> Text
printAtom (Symbol s) = s
printAtom (LitInt n) = T.pack . show $ n
-- `show` gives us char escapes and quotes for free
printAtom (LitStr s) = T.pack . show $ s

parserSpec :: SExprParser Atom (SExpr Atom)
parserSpec = mkAtomParser
    [ atom Symbol ident
    , atom (LitInt . fromInteger) signedDecNumber
    ]
    & addReader '[' vecReader
    & addReader '\'' (mkQuoter "quote")
    & addReader '`' (mkQuoter "quasiquote")
    & addReader ',' (mkQuoter "unquote")
    & setComment (lineComment ";"
              <|> simpleBlockComment "#|" "|#")

parser :: SExprParser Atom Sexp
parser = setCarrier (Right . marshal) (asWellFormed parserSpec)

printer :: SExprPrinter Atom Sexp
printer = basicPrint printAtom
        & setFromCarrier (fromWellFormed . marshalBack)

--------------------------------------------------------------------------------

parse1 :: Text -> Either String Sexp
parse1 = decodeOne parser

parse :: Text -> Either String [Sexp]
parse = decode parser

pprint :: Sexp -> Text
pprint = encodeOne printer

