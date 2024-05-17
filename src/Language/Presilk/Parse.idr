module Language.Presilk.Parse
--------------------------------------------------------------------------------
import Text.Parser
import Text.Lexer
import Derive.Prelude
import Language.Presilk.Lex
import public Language.Presilk.Syntax
--------------------------------------------------------------------------------
%hide Language.Reflection.Derive.match
%default total
%language ElabReflection
--------------------------------------------------------------------------------

P : Bool -> Type -> Type
P c a = Grammar () SilkToken c a

implementation {c : _} -> Functor (P c) where
    map f p = map f p

atom : P True Atom
atom = (Symbol <$> match Token.Symbol)
   <|> (LitInt <$> match Token.LitInt)
   <|> (LitStr <$> match Token.LitStr)

mutual
  app : Sexp -> P True Sexp
  app f = do
    x <- sexp
    app1 $ App f x

  app1 : Sexp -> P False Sexp
  app1 e = app e <|> pure e

  list : P True Sexp
  list = do
      match Token.LParen
      commit 
      e <- sexp
      e' <- app e
      match Token.RParen
      pure e'

  makeQuoter : String -> (k : TokenKind)
            -> {auto h : TokType k = ()} -> P True Sexp
  makeQuoter {h} s k =
      (q >> sexp')
    where
      sexp' : P True Sexp
      sexp' = App (Atomic (Symbol s)) <$> sexp

      q : P True ()
      q = do
          r <- match k
          pure $ rewrite sym h in r

  quotation : P True Sexp
  quotation = makeQuoter "qupte" Token.Quote
          <|> makeQuoter "quasiquote" Token.Quasiquote
          <|> makeQuoter "unquote" Token.Unquote

  sexp : P True Sexp
  sexp = (Atomic <$> atom)
     <|> list
     <|> quotation

export
parseSilk : List (WithBounds SilkToken) -> Either String Sexp
parseSilk ts =
  case parse sexp $ filter (not . ignored) ts of
    Right (l, []) => Right l
    Right e       => Left "contains tokens that were not consumed"
    Left e        => Left $ show e

