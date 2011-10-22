{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Yesod.Admin.Crud where

import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax

import Database.Persist.Base (EntityDef(..), SomePersistField(..), PersistValue(..), ColumnDef(columnName))
import Text.Cassius (cassiusFile)
import Text.Hamlet (shamletFile)
import Yesod

data Admin = Admin

getAdmin :: x -> Admin
getAdmin = const Admin

-- Acronym class for YesodAdmin requirements
class (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler Admin master IO)) => YesodAdminReqs master
instance (Yesod master, YesodPersist master, PersistBackend (YesodPersistBackend master) (GGHandler Admin master IO)) => YesodAdminReqs master

type Row = (PersistValue, [SomePersistField])

data TableFuncs master = TableFuncs
    { getRow :: Text -> GHandler Admin master [SomePersistField]
    , listRows :: GHandler Admin master [Row]
    , addRow :: GHandler Admin master ()
    , editRow :: Text -> GHandler Admin master ()
    , deleteRow :: Text -> GHandler Admin master ()
    , tableDef :: EntityDef
    }

class YesodAdminReqs master => YesodAdmin master where
    getTableNames :: GHandler Admin master [String]
    getTableFuncs :: String -> GHandler Admin master (TableFuncs master)

mkYesodSub "Admin" [ClassP ''YesodAdmin [VarT $ mkName "master"]] [parseRoutes|
/ ShowR GET
/add/#String AddR POST
/edit/#String/#Text EditR GET POST
/delete/#String/#Text DeleteR GET POST
|]

getShowH :: YesodAdmin master => GHandler Admin master [(String, [String], [Row])]
getShowH =
    getTableNames >>= mapM tableData
    where
        tableData table = do
            tableFuncs <- getTableFuncs table
            rows <- listRows tableFuncs
            let cols = map columnName . entityColumns $ tableDef tableFuncs
            return (table, cols, rows)

genericIntercalate :: Monoid a => a -> [a] -> a
genericIntercalate _ [] = mempty
genericIntercalate sep (x : xs) = mconcat (x : map (mappend sep) xs)

tableHeader :: String -> [String] -> Int -> Html
tableHeader title colNames numActions =
    $(shamletFile "hamlet/table-header.hamlet")
    where
        hasActions = numActions > 0
        numCols = 1 + length colNames + numActions

tableRowValues :: [SomePersistField] -> Html
tableRowValues vals = $(shamletFile "hamlet/table-row-vals.hamlet")

isEmptyCell :: PersistField a => a -> Bool
isEmptyCell = (PersistNull ==) . toPersistValue

stylesheet :: GWidget Admin master ()
stylesheet = addCassius $(cassiusFile "cassius/yesod-admin-crud.css")

getShowR :: YesodAdmin master => GHandler Admin master RepHtml
getShowR = do
    tables <- getShowH
    adminRoute <- getRouteToMaster
    defaultLayout $ do
        stylesheet
        mapM_ (showTable adminRoute) tables
    where
        showTable adminRoute (entity, colNames, items) =
            $(whamletFile "hamlet/listing.hamlet")
            where
                title = entity ++ " Table"

getEditR :: YesodAdmin master => String -> Text -> GHandler Admin master RepHtml
getEditR table key = do
    tableFuncs <- getTableFuncs table
    let cols = map columnName . entityColumns $ tableDef tableFuncs
    item <- getRow tableFuncs key
    adminRoute <- getRouteToMaster
    defaultLayout $ do
        stylesheet
        $(whamletFile "hamlet/edit-row.hamlet")
    where
        cellText (_, cell) = if isEmptyCell cell then mempty else toSinglePiece (toPersistValue cell)

getDeleteR :: YesodAdmin master => String -> Text -> GHandler Admin master RepHtml
getDeleteR table key = do
    tableFuncs <- getTableFuncs table
    let cols = map columnName . entityColumns $ tableDef tableFuncs
    item <- getRow tableFuncs key
    adminRoute <- getRouteToMaster
    defaultLayout $ do
        stylesheet
        $(whamletFile "hamlet/delete-row.hamlet")

directHome :: YesodAdmin master => GHandler Admin master ()
directHome = do
    adminRoute <- getRouteToMaster
    redirect RedirectTemporary (adminRoute ShowR)

postAddR :: YesodAdmin master => String -> GHandler Admin master ()
postAddR table = do
    tableFuncs <- getTableFuncs table
    addRow tableFuncs
    directHome

postEditR :: YesodAdmin master => String -> Text -> GHandler Admin master ()
postEditR table key = do
    tableFuncs <- getTableFuncs table
    editRow tableFuncs key
    directHome

postDeleteR :: YesodAdmin master => String -> Text -> GHandler Admin master ()
postDeleteR table key = do
    tableFuncs <- getTableFuncs table
    deleteRow tableFuncs key
    directHome

type SelectAllRes master v = [(Key (YesodPersistBackend master) v, v)]

helpers :: (PersistEntity v, YesodAdmin master, SinglePiece (Key (YesodPersistBackend master) v)) => v -> GHandler Admin master (TableFuncs master)
helpers dummy =
    return TableFuncs
    { getRow = fmap (toPersistFields . fromJust) . runDB . get . keyFromText
    , listRows = fmap (map toRow . itemsOfDummy dummy) . runDB $ selectList [] [LimitTo 16]
    , addRow = getAddedVal >>= fmap (const () . keyOfDummy dummy) . runDB . insert
    , editRow = (getAddedVal >>=) . (runDB .) . replace . keyFromText
    , deleteRow = runDB . delete . keyFromText
    , tableDef = def
    }
    where
        keyFromText = keyOfDummy dummy . fromJust . fromSinglePiece
        keyOfDummy :: v -> Key a v -> Key a v
        keyOfDummy = const id
        itemsOfDummy :: v -> [(Key a v, v)] -> [(Key a v, v)]
        itemsOfDummy = const id
        def = entityDef dummy
        getAddedVal = fmap (fromRight . fromPersistValues) . mapM (getPersistValueFromPost . columnName) $ entityColumns def
        getPersistValueFromPost = fmap (fromJust . fromSinglePiece . fromJust) . lookupPostParam . fromString
        fromRight (Right x) = x
        fromRight _ = undefined
        toRow (key, val) = (toPersistValue key, toPersistFields val)

mkYesodAdmin :: [EntityDef] -> Q [Dec]
mkYesodAdmin defs =
    return [ InstanceD context (AppT (ConT ''YesodAdmin) master) body ]
    where
        context = [ClassP ''YesodAdminReqs [master]] ++ map singlePiece defs
        master = VarT (mkName "master")
        singlePiece def = ClassP ''SinglePiece [foldl1 AppT [ConT ''Key, AppT (ConT ''YesodPersistBackend) master, ConT . mkName $ entityName def]]
        body =
            [ FunD 'getTableNames [Clause [] (NormalB getTableNamesBody) []]
            , FunD 'getTableFuncs (map clause defs ++ [invalid])
            ]
        tableName = LitE . StringL . entityName
        getTableNamesBody = AppE (VarE 'return) . ListE $ map tableName defs
        clause def = Clause [LitP . StringL $ entityName def] (NormalB (getTableFuncsBody def)) []
        invalid = Clause [WildP] (NormalB (VarE 'undefined)) []
        getTableFuncsBody def = AppE (VarE 'helpers) . SigE (VarE 'undefined) . ConT . mkName $ entityName def
