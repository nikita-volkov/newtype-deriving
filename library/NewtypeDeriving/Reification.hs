module NewtypeDeriving.Reification where

import BasePrelude
import Language.Haskell.TH



data Newtype =
  Newtype {
    newtypeTypeName :: Name,
    newtypeConstructorName :: Name,
    newtypeInnerType :: Type
  }
  deriving (Show)

reifyNewtype :: Name -> Q (Either String Newtype)
reifyNewtype =
  fmap parseInfo . reify
  where
    parseInfo =
      \case
        TyConI (NewtypeD _ typeName _ con derivations) -> do
          (conName, innerType) <-
            case con of
              NormalC n [(_, t)] -> Right (n, t)
              RecC n [(_, _, t)] -> Right (n, t)
              _ -> Left $ "Invalid constructor: " <> show con
          return $ Newtype typeName conName innerType
        i ->
          Left $ "Invalid type of a name"

-- |
-- Given a kind @* -> *@ type, 
-- peel off a kind @(* -> *) -> (* -> *)@ type (the monad-transformer)
-- and another @* -> *@ type (the inner monad).
peelTransformer :: Type -> Maybe (Type, Type)
peelTransformer =
  \case
    AppT t m -> Just (t, m)
    _ -> Nothing

