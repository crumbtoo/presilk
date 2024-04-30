{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Presilk.Syntax
    (
    -- * S-expression AST
     Sexp(..), SexpF(..), Atom(..)
    , Symbol
    -- * Pattern synonyms
    , pattern Vector
    -- * Syntax tree manipulations
    , slurpb, sexpApply
    -- * Optics
    , _Atom, _List
    , _Symbol, _LitInt, _LitStr
    )
    where
--------------------------------------------------------------------------------
import Data.Text                    qualified as T
import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.String
import GHC.Exts                     (IsList(..))

import Control.Lens                 hiding (List)
--------------------------------------------------------------------------------

type Symbol = T.Text

data Sexp = Atom Atom
          | List [Sexp]
          deriving (Show, Eq)

pattern Vector :: [Sexp] -> Sexp
pattern Vector xs = List (Atom (Symbol "vector") : xs)

data Atom = Symbol Symbol
          | LitInt Int
          | LitStr T.Text
          deriving (Show, Eq)

-- convenient syntax
instance IsList Sexp where
  type Item Sexp = Sexp
  toList (List es) = es
  toList (Atom a) = error "called toList on an atom"

  fromList = List

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

--------------------------------------------------------------------------------

makePrisms ''Sexp
makePrisms ''Atom

