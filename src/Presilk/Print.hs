{-# LANGUAGE OverloadedStrings #-}
module Presilk.Print
    where
--------------------------------------------------------------------------------
import Data.Text                  qualified as T
import Data.Text                  (Text)

import Data.Functor.Foldable

import Presilk.Syntax
--------------------------------------------------------------------------------

prettyPrint :: Sexp -> Text
prettyPrint = cata prettyPrintF

prettyPrintF :: SexpF Text -> Text
prettyPrintF (ListF es) = "(" <> T.unwords es <> ")"
prettyPrintF (AtomF (Symbol s)) = s
prettyPrintF (AtomF (LitInt s)) = T.pack . show $ s
-- `show` gives us character escaping and quotation marks for free
prettyPrintF (AtomF (LitStr s)) = T.pack . show $ s

