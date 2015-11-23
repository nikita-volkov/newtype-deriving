{-# LANGUAGE CPP #-}
module NewtypeDeriving.TH where

import BasePrelude
import Language.Haskell.TH


purifyQ :: Q a -> a
purifyQ = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 

{-# INLINE classP #-}
classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl =
  foldl AppT (ConT n) tl
#else
classP =
  ClassP
#endif
