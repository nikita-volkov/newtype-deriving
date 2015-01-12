{-# LANGUAGE UndecidableInstances, TypeFamilies, FlexibleInstances,
             TemplateHaskell, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses #-}
module Main where

import Prelude ()
import BasePrelude
import NewtypeDeriving
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer


main =
  return ()


newtype T m a =
  T (ReaderT Int (StateT Char (WriterT [Int] (EitherT String (MaybeT m)))) a)
  deriving (Functor, Applicative, Monad)

monadTransInstance ''T
monadTransControlInstance ''T
monadBaseTransformerInstance ''T
monadBaseControlTransformerInstance ''T

