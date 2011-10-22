{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Yesod.Admin.Crud.Type (Admin(..), YesodAdmin(..), YesodAdminReqs) where

import Database.Persist.Base(PersistBackend)
import Yesod (GGHandler, GHandler, Yesod, YesodPersist, YesodPersistBackend)

import Yesod.Admin.Crud.TableFuncs (TableFuncs)

data Admin = Admin

-- Acronym class for YesodAdmin requirements. Used in TH code.
class (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler Admin master IO)) => YesodAdminReqs master
instance (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler Admin master IO)) => YesodAdminReqs master

class YesodAdminReqs master => YesodAdmin master where
    getTableNames :: GHandler Admin master [String]
    getTableFuncs :: String -> GHandler Admin master (TableFuncs Admin master)
