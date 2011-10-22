module Yesod.Admin.Crud (Admin, getAdmin, mkYesodAdmin) where

import Yesod.Admin.Crud.Handlers ()
import Yesod.Admin.Crud.TH (mkYesodAdmin)
import Yesod.Admin.Crud.Type (Admin(..))

getAdmin :: x -> Admin
getAdmin = const Admin
