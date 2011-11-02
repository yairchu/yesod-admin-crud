{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Yesod.Admin.Crud.Type (Admin(..), YesodAdminReqs) where

import Yesod.Admin.User (YesodAdminUser)

import Database.Persist.Base (PersistBackend)
import Yesod (GGHandler, Yesod, YesodPersist, YesodPersistBackend)

data Admin = Admin

-- Acronym class for YesodAdmin requirements.
-- (This would be nicer with ConstraintKinds, which is coming up in GHC 7.4)
class
    ( Yesod master
    , YesodAdminUser master
    , YesodPersist master
    , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)
    ) => YesodAdminReqs sub master
instance
    ( Yesod master
    , YesodAdminUser master
    , YesodPersist master
    , PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)
    ) => YesodAdminReqs sub master
