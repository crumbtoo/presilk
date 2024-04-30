{-# LANGUAGE OverloadedLists, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- temp because i don't have hls configured properly lmfao
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Presilk.Compile
    ( compile
    )
    where
--------------------------------------------------------------------------------
import Control.Monad.State
import Control.Monad.Except
import Control.Monad
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.HashMap.Strict          qualified as H
import Text.Read                    (readMaybe)

import Control.Lens                 hiding (List, uncons)
import Control.Lens.Extras

import Data.Functor.Foldable

import Language.Lua.Syntax          (Name(..), Block (Block), Exp)
import Language.Lua.Syntax          qualified as L
import Language.Lua.PrettyPrinter   (pprint)

import Presilk.Syntax
import Presilk.Parse
--------------------------------------------------------------------------------

type C = StateT [Name] (Except Text)

freshName :: C Name
freshName = state (fromJust . uncons)

liftBlock :: L.Block -> L.Exp
liftBlock = L.EFunDef . L.FunBody [] False

unliftBlock :: L.Exp -> L.Block
unliftBlock e = L.Block [] (Just [e])

pattern VarE :: Name -> L.Exp
pattern VarE n = L.PrefixExp (L.PEVar (L.VarName n))

pattern Return :: L.Exp -> L.Block
pattern Return e = L.Block [] (Just [e])

-- > Number IntNum :: Text -> Exp
-- who made this fucking library
pattern IntE :: Int -> L.Exp
pattern IntE n <- L.Number L.IntNum (readMaybe . T.unpack -> Just n)
  where IntE n = L.Number L.IntNum (T.pack . show $ n)

--------------------------------------------------------------------------------

class Lua a where
    toBlock :: a -> Block
    toExp  :: a -> Exp

    fromExp :: Exp -> a
    fromBlock :: Block -> a

instance Lua Exp where
    toBlock = Return
    toExp = id

    fromExp = id
    fromBlock = toExp

instance Lua Block where
    toBlock = id
    toExp b = L.PrefixExp . L.PEFunCall $ L.NormalFunCall f (L.Args [])
      where f = L.Paren . L.EFunDef . L.FunBody [] False $ b

    fromBlock = id
    fromExp = toBlock

--------------------------------------------------------------------------------

type Env = H.HashMap Symbol Name

compileOne :: Lua a => Env -> Sexp -> C a

compileOne _ (Atom (LitInt n)) =
  pure . fromExp . IntE $ n

compileOne _ (Atom (LitStr s)) =
  pure . fromExp . L.String $ s

compileOne g (Atom (Symbol s)) =
  case H.lookup s g of
    Just a -> pure . fromExp $ VarE a
    Nothing -> throwError $ "undefined symbol: " <> s

-- (def ...) appeared in a meaningless context
compileOne _ (List ("def":_)) = pure . fromExp $ L.Nil

compileOne g (List ["lambda", Vector as, e])
  | Just syms <- as ^? below (_Atom . _Symbol) = do
    names <- genNames syms
    let g' = augmentEnv g (syms `zip` names)
    e' <- compileOne @Block g' e
    pure . fromExp . L.EFunDef . L.FunBody names False $ e'
  | otherwise = throwError "syntax error in lambda form"

compileOne g (List ("+" : as)) = case as of
  [] -> pure . fromExp . IntE $ 0
  _ -> fromExp . foldr1 (L.Binop L.Add) <$> compileOne @Exp g `traverse` as

compileOne _ e = error . show $ e

genNames :: [Symbol] -> C [Name]
genNames = traverse (const freshName)

augmentEnv :: Env -> [(Symbol, Name)] -> Env
augmentEnv = foldr $ \(s,n) g -> H.insert s n g

--------------------------------------------------------------------------------


compile :: [Sexp] -> Either Text [L.Block]
compile = evalC . traverse _

compileTest s = case parse1 s of
  Left e -> Left . T.pack . show $ e
  Right a -> evalC . fmap pprint . compileOne @Exp mempty $ a

evalC :: C a -> Either Text a
evalC = runExcept . (`evalStateT` ns)
  where ns = [ Name $ "_" <> T.pack (show n) | n <- [0..] ]

