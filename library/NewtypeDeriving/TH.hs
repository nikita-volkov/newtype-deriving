module NewtypeDeriving.TH where

import BasePrelude
import Language.Haskell.TH


purifyQ :: Q a -> a
purifyQ = unsafePerformIO . runQ

tryToReify :: Name -> Q (Maybe Info)
tryToReify n = recover (return Nothing) (fmap Just $ reify n) 
