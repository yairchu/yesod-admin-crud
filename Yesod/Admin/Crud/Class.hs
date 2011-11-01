{-# LANGUAGE FlexibleContexts #-}

module Yesod.Admin.Crud.Class (YesodAdmin(..)) where

import Yesod.Admin.Crud.TableFuncs (TableFuncs)
import Yesod.Admin.Crud.Type (Admin, YesodAdminReqs)

import Yesod (GHandler)

class YesodAdminReqs Admin master => YesodAdmin master where
    getTableNames :: GHandler Admin master [String]
    getTableFuncs :: String -> GHandler Admin master (TableFuncs Admin master)
