{-# LANGUAGE TemplateHaskell #-}
module Presilk.Syntax
    (
    -- * S-expression AST
     Sexp(..), SexpF(..), Atom(..)
    , Symbol
    -- * Syntax tree manipulations
    , slurpb, sexpApply
    )
    where
--------------------------------------------------------------------------------
import Data.Text                    qualified as T
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.String
--------------------------------------------------------------------------------

type Symbol = T.Text

data Sexp = Atom Atom
          | List [Sexp]
          deriving Show

data Atom = Symbol Symbol
          | LitInt Int
          | LitStr T.Text
          deriving Show

instance IsString Sexp where
  fromString = Atom . Symbol . T.pack

instance IsString Atom where
  fromString = Symbol . T.pack

makeBaseFunctor ''Sexp
deriving instance Show a => Show (SexpF a)
deriveShow1 ''SexpF

-- | Slurp backwards
--     a (b c) -> (a b c)
slurpb :: Sexp -> Sexp -> Sexp
slurpb a (List as) = List (a:as)

-- | Apply a sexp to a single argument
--     a b -> (a b)
sexpApply :: Sexp -> Sexp -> Sexp
sexpApply a b = List [a,b]

