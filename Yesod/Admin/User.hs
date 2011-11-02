module Yesod.Admin.User (YesodAdminUser(..)) where

import Yesod (GHandler)

class YesodAdminUser master where
    isAdminUser :: GHandler sub master Bool
