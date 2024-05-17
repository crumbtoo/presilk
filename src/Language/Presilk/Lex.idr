module Language.Presilk.Lex
--------------------------------------------------------------------------------
import Text.Lexer
import Language.Presilk.Syntax
import Derive.Prelude
import public Text.Lexer
--------------------------------------------------------------------------------
%default total
%language ElabReflection
--------------------------------------------------------------------------------

namespace Token
  public export
  data TokenKind = LParen
                 | RParen
                 | Symbol
                 | LitInt
                 | LitStr
                 | LSquare
                 | RSquare
                 | Quote
                 | Quasiquote
                 | Unquote
                 | SexpComment
                 | Ignore

  %runElab derive "TokenKind" [Show, Eq]

  export
  implementation TokenKind TokenKind where
    TokType Symbol = String
    TokType LitInt = Int
    TokType LitStr = String
    TokType _      = ()

    tokValue LParen _      = ()
    tokValue RParen _      = ()
    tokValue LSquare _     = ()
    tokValue RSquare _     = ()
    tokValue SexpComment _ = ()
    tokValue Quote _       = ()
    tokValue Quasiquote _  = ()
    tokValue Unquote _     = ()
    tokValue Symbol s      = s
    tokValue Ignore _      = ()
    tokValue LitInt n      = cast n
    tokValue LitStr s      = s

public export
SilkToken : Type
SilkToken = Token TokenKind

export
implementation Show SilkToken where
  show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

export
ignored : WithBounds SilkToken -> Bool
ignored (MkBounded (Tok Ignore _) _ _)      = True
ignored (MkBounded (Tok SexpComment _) _ _) = True
ignored _                                   = False

symbol' : Lexer
symbol' = pred isHeadChar <+> many (pred isSymChar)
  where
    cs : List Char
    cs = ['(',')','[',']',',','`','"']
    isSymChar c = (not (c `elem` cs))
               && (not . isSpace $ c)
    isHeadChar c = isSymChar c && (c /= '\'') && not (isDigit c)

silkTokenMap : TokenMap SilkToken
silkTokenMap = toTokenMap
    [ (spaces,                                 Ignore)
    , (lineComment (exact ";"),                Ignore)
    , (blockComment (exact "#|") (exact "#|"), Ignore)
    , (exact "(",                              LParen)
    , (exact ")",                              RParen)
    , (exact "[",                              LSquare)
    , (exact "]",                              RSquare)
    , (exact "#;",                             SexpComment)
    , (exact "'",                              Quote)
    , (exact "`",                              Quasiquote)
    , (exact ",",                              Unquote)
    , (stringLit,                              LitStr)
    , (intLit,                                 LitInt)
    ]
    ++
    [ (symbol', Tok Symbol)
    ]

export
lexSilk : String -> Either String (List (WithBounds SilkToken))
lexSilk s = case lex silkTokenMap s of
  (ts, _, _, "") => Right ts
  _              => Left "aww"

