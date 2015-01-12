module NewtypeDeriving.Rendering where

import BasePrelude
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import NewtypeDeriving.TH
import Language.Haskell.TH


monadTransInstance 
  :: Type   -- ^ Type of Newtype
  -> Name   -- ^ Constructor name
  -> Int    -- ^ Amount of inner layers
  -> Dec
monadTransInstance transformerType conName layersCount =
  head $ purifyQ $
  [d|
    instance MonadTrans $(pure transformerType) where
      lift =
        $(pure liftExp)
  |]
  where
    liftExp =
      foldl' composeExp (ConE conName) $ replicate layersCount (VarE 'lift)

monadBaseTransformerInstance
  :: Type
  -> Name
  -> Dec
monadBaseTransformerInstance transformerType conName =
  head $ purifyQ $
  [d|
    instance MonadBase b m => MonadBase b ($(pure transformerType) m) where
      liftBase =
        $(conE conName) . liftBase
  |]

monadTransControlInstance 
  :: Type   -- ^ Type of Newtype
  -> Name   -- ^ Constructor name
  -> [Type] -- ^ Layers of inner transfomer types
  -> Dec
monadTransControlInstance transformerType conName layers =
  head $ purifyQ $
  [d|
    instance MonadTransControl $(pure transformerType) where
      type StT $(pure transformerType) $(varT stArgName) = 
        $(pure stType)
      liftWith =
        $(pure liftWithExp)
      restoreT =
        $(pure restoreTExp)
  |]
  where
    restoreTExp =
      foldl' composeExp (ConE conName) $ 
      replicate (length layers) (VarE 'restoreT)
    liftWithExp =
      LamE [VarP onUnliftVarName] $
      AppE (ConE conName) $
      foldr' (\n -> AppE (VarE 'liftWith) . LamE [VarP n])
             (AppE (VarE onUnliftVarName) unliftExp)
             (unliftNames)
      where
        onUnliftVarName =
          mkName "onUnlift"
        unliftNames =
          zipWith (\_ i -> mkName $ "unlift" <> show i) layers [1..]
        unliftExp =
          foldr' composeExp unwrapExp $ map VarE $ reverse unliftNames
        unwrapExp =
          LamE [ConP conName [VarP mVarName]] (VarE mVarName)
          where
            mVarName = 
              mkName "m"
    stArgName =
      mkName "a"
    stType =
      foldl' (flip AppT) (VarT stArgName) $ map (AppT (ConT ''StT)) layers

monadBaseControlTransformerInstance
  :: Type
  -> Dec
monadBaseControlTransformerInstance t =
  head $ purifyQ $
  [d|
    instance MonadBaseControl b m => MonadBaseControl b ($(pure t) m) where
      type StM ($(pure t) m) a = ComposeSt $(pure t) m a
      liftBaseWith = defaultLiftBaseWith
      restoreM = defaultRestoreM
  |]

composeExp :: Exp -> Exp -> Exp
composeExp a b =
  UInfixE a (VarE '(.)) b
