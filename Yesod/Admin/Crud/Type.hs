{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Yesod.Admin.Crud.Type (Admin(..), YesodAdminReqs) where

import Database.Persist.Base (PersistBackend)
import Yesod (GGHandler, Yesod, YesodPersist, YesodPersistBackend)

data Admin = Admin

-- Acronym class for YesodAdmin requirements. Used in TH code.
class (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => YesodAdminReqs sub master
instance (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler sub master IO)) => YesodAdminReqs sub master
