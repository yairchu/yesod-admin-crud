{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

import qualified Database.Persist.Base as P
import Database.Persist.GenericSql
import Database.Persist.Sqlite
import Data.Text
import Yesod
import Yesod.Admin.Crud
import Yesod.Default.Config
import Yesod.Default.Main

data Example = Example { connPool :: P.PersistConfigPool SqliteConf }

instance YesodPersist Example where
    type YesodPersistBackend Example = SqlPersist
    runDB f = liftIOHandler $ fmap connPool getYesod >>= P.runPool (undefined :: SqliteConf) f

mkYesod "Example" [parseRoutes|
/admin AdminR Admin getAdmin
|]

instance Yesod Example where
    approot _ = "http://localhost:3000"

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkYesodAdmin] [persist|
ExampleA
    aa Text
    ab Text Maybe
    UniqueExampleA aa
ExampleB
    ba Int
    UniqueExampleB ba
|]

withExample conf _ f = do
    dbconf <- withYamlEnvironment "example-sqlite.yml" (appEnv conf) $ either error return . P.loadConfig
    P.withPool (dbconf :: SqliteConf) $ \p -> do
        P.runPool dbconf (runMigration migrateAll) p
        defaultRunner f $ Example p

main :: IO ()
main = defaultMain fromArgs withExample
