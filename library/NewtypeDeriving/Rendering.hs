module NewtypeDeriving.Rendering where

import BasePrelude
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import NewtypeDeriving.TH
import Language.Haskell.TH hiding (classP)


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

-- |
-- Produces a declaration in the spirit of the following:
-- @
-- instance MonadBase b m => MonadBase b ($(pure transformerT) m) where
--   liftBase =
--     $(conE conName) . liftBase
-- @
monadBaseTransformerInstance
  :: Type
  -> Name
  -> Dec
monadBaseTransformerInstance transformerT conName =
  InstanceD cxt type_ decs
  where
    baseMonadName =
      mkName "b"
    mainMonadName =
      mkName "m"
    baseMonadT =
      VarT baseMonadName
    mainMonadT =
      VarT mainMonadName
    cxt =
      [pred]
      where
        pred =
          classP ''MonadBase [baseMonadT, mainMonadT]
    type_ =
      foldl1 AppT [ConT ''MonadBase, baseMonadT, AppT transformerT mainMonadT]
    decs =
      [liftBaseD]
      where
        liftBaseD =
          FunD 'liftBase [clause]
          where
            clause =
              Clause [] body []
              where
                body =
                  NormalB exp
                  where
                    exp =
                      composeExp (ConE conName) (VarE 'liftBase)

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

-- |
-- Produces a declaration in the spirit of the following:
-- @
-- instance MonadBaseControl b m => MonadBaseControl b ($(pure transformerT) m) where
--   type StM ($(pure transformerT) m) a = ComposeSt $(pure transformerT) m a
--   liftBaseWith = defaultLiftBaseWith
--   restoreM = defaultRestoreM
-- @
monadBaseControlTransformerInstance
  :: Type
  -> Dec
monadBaseControlTransformerInstance transformerT =
  InstanceD cxt type_ decs
  where
    baseMonadName =
      mkName "b"
    mainMonadName =
      mkName "m"
    resultName =
      mkName "a"
    baseMonadT =
      VarT baseMonadName
    mainMonadT =
      VarT mainMonadName
    resultT =
      VarT resultName
    cxt =
      [pred]
      where
        pred =
          classP ''MonadBaseControl [baseMonadT, mainMonadT]
    type_ =
      AppT (AppT (ConT ''MonadBaseControl) baseMonadT) (AppT transformerT mainMonadT)
    decs =
      [stMD, liftBaseWithD, restoreMD]
      where
        stMD =
          TySynInstD ''StM eqn
          where
            eqn =
              TySynEqn [AppT transformerT mainMonadT, resultT] rhsT
              where
                rhsT =
                  foldl1 AppT [ConT ''ComposeSt, transformerT, mainMonadT, resultT]
        liftBaseWithD =
          FunD 'liftBaseWith [clause]
          where
            clause =
              Clause [] body []
              where
                body =
                  NormalB exp
                  where
                    exp =
                      VarE 'defaultLiftBaseWith
        restoreMD =
          FunD 'restoreM [clause]
          where
            clause =
              Clause [] body []
              where
                body =
                  NormalB exp
                  where
                    exp =
                      VarE 'defaultRestoreM

composeExp :: Exp -> Exp -> Exp
composeExp a b =
  UInfixE a (VarE '(.)) b
