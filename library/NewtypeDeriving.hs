-- |
-- This module provides
-- Template Haskell based derivers for typical newtype instances, 
-- which the @GeneralizedNewtypeDeriving@ extension refuses to handle. 
-- 
-- Here is what it allows you to do:
-- 
-- >{-# LANGUAGE UndecidableInstances, TypeFamilies, FlexibleInstances,
-- >             TemplateHaskell, GeneralizedNewtypeDeriving,
-- >             MultiParamTypeClasses #-}
-- >
-- >import NewtypeDeriving
-- >import Control.Monad.Base
-- >import Control.Monad.Trans.Control
-- >import Control.Monad.Trans.Class
-- >import Control.Monad.Trans.Either
-- >import Control.Monad.Trans.Maybe
-- >import Control.Monad.Trans.State
-- >import Control.Monad.Trans.Reader
-- >import Control.Monad.Trans.Writer
-- >
-- >newtype T m a =
-- >  T (ReaderT Int (StateT Char (WriterT [Int] (EitherT String (MaybeT m)))) a)
-- >  deriving (Functor, Applicative, Monad)
-- >
-- >monadTransInstance ''T
-- >monadTransControlInstance ''T
-- >monadBaseTransformerInstance ''T
-- >monadBaseControlTransformerInstance ''T
module NewtypeDeriving where

import BasePrelude
import Language.Haskell.TH
import qualified NewtypeDeriving.Reification as Reification
import qualified NewtypeDeriving.Rendering as Rendering


-- |
-- Given a name of a newtype wrapper 
-- produce an instance of
-- @Control.Monad.Trans.Class.'Control.Monad.Trans.Class.MonadTrans'@.
monadTransInstance :: Name -> Q [Dec]
monadTransInstance n =
  do
    Reification.Newtype typeName conName innerType <-
      join $ fmap (either fail return) $
      Reification.reifyNewtype n
    let
      layers =
        unfoldr Reification.peelTransformer $ 
        case innerType of AppT m _ -> m
    return $ pure $
      Rendering.monadTransInstance (ConT typeName) conName (length layers)

-- |
-- Given a name of a newtype wrapper 
-- produce an instance of
-- @Control.Monad.Base.'Control.Monad.Base.MonadBase'@,
-- which is specialised for monad transformers.
monadBaseTransformerInstance :: Name -> Q [Dec]
monadBaseTransformerInstance n =
  do
    Reification.Newtype typeName conName innerType <-
      join $ fmap (either fail return) $
      Reification.reifyNewtype n
    return $ pure $
      Rendering.monadBaseTransformerInstance (ConT typeName) conName

-- |
-- Given a name of a newtype wrapper 
-- produce an instance of
-- @Control.Monad.Trans.Control.'Control.Monad.Trans.Control.MonadTransControl'@.
monadTransControlInstance :: Name -> Q [Dec]
monadTransControlInstance n =
  do
    Reification.Newtype typeName conName innerType <-
      join $ fmap (either fail return) $
      Reification.reifyNewtype n
    let
      layers =
        unfoldr Reification.peelTransformer $ 
        case innerType of AppT m _ -> m
    return $ pure $ 
      Rendering.monadTransControlInstance (ConT typeName) conName layers

-- |
-- Given a name of a newtype wrapper 
-- produce an instance of
-- @Control.Monad.Trans.Control.'Control.Monad.Trans.Control.MonadBaseControl'@,
-- which is specialised for monad transformers.
monadBaseControlTransformerInstance :: Name -> Q [Dec]
monadBaseControlTransformerInstance n =
  return $ pure $
    Rendering.monadBaseControlTransformerInstance (ConT n)
