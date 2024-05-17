module Language.Presilk.Syntax
--------------------------------------------------------------------------------
import Derive.Prelude
--------------------------------------------------------------------------------
%prefix_record_projections off
%language ElabReflection
--------------------------------------------------------------------------------

public export
data Atom = Symbol String
          | LitInt Int
          | LitStr String

public export
data Sexp = Atomic Atom
          | App Sexp Sexp

public export
record Module where
  constructor MkModule

%runElab derive "Atom" [Show, Eq]
%runElab derive "Sexp" [Show, Eq]
%runElab derive "Module" [Show, Eq]

