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
import Text.Hamlet (shamlet)
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
tableHeader title colNames numActions = [shamlet|
    <tr .yesod-admin-listing.title>
        <th colspan=#{numCols}>
            #{title}
    <tr .yesod-admin-listing.headers>
        <th .yesod-admin-listing.headers>
            Key
        $forall col <- colNames
            <th .yesod-admin-listing.headers>
                #{col}
        $if hasActions
            <th colspan=#{numActions} .yesod-admin-listing.headers>
                Actions
    |]
    where
        hasActions = numActions > 0
        numCols = 1 + length colNames + numActions

tableRowValues :: [SomePersistField] -> Html
tableRowValues vals = [shamlet|
    $forall val <- vals
        $if isEmptyCell val
            <td .yesod-admin-listing.values.empty> N/A
        $else
            <td .yesod-admin-listing.values.value>
                #{toSinglePiece (toPersistValue val)}
    |]

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
        showTable adminRoute (entity, colNames, items) = do
            [whamlet|
                <table .yesod-admin-listing>
                    #{tableHeader title colNames 2}
                    $forall item <- items
                        <tr .yesod-admin-listing.values>
                            <td .yesod-admin-listing.values.key>
                                #{toSinglePiece (fst item)}
                            #{tableRowValues (snd item)}
                            <td .yesod-admin-listing.values.action.edit>
                                <form method=get action=@{adminRoute (EditR entity (toSinglePiece (fst item)))}>
                                    <input type=submit value="Edit" .yesod-admin-listing.values.action.edit>
                            <td .yesod-admin-listing.values.action.delete>
                                <form method=get action=@{adminRoute (DeleteR entity (toSinglePiece (fst item)))}>
                                    <input type=submit value="Delete" .yesod-admin-listing.values.action.delete>
                    <form method=post action=@{adminRoute (AddR entity)}>
                        <tr .yesod-admin-listing.add-row>
                            <td .yesod-admin-listing.values.key.new-row>
                                New:
                            $forall col <- colNames
                                <td .yesod-admin-listing.values.value.new-row>
                                    <textarea name="#{col}" rows=1 cols=10 .yesod-admin-listing.values.value.new-row>
                            <td colspan=2 onclick="add_#{entity}()" id="add-#{entity}" .yesod-admin-listing.values.action.add>
                                <input type=submit value="Add" .yesod-admin-listing.values.action.add>
                |]
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
        [whamlet|
            <form method=post action=@{adminRoute (EditR table key)}>
                <table .yesod-admin-listing>
                    #{tableHeader table cols 0}
                    <tr .yesod-admin-listing.values>
                        <td .yesod-admin-listing.values.key>
                            #{key}
                        $forall val <- zip cols item
                            <td .yesod-admin-listing.values.key>
                                <textarea name="#{fst val}" rows=1 cols=#{max 10 (Text.length (cellText val))} .yesod-admin-listing.values.value.new-row>
                                    #{cellText val}
                <a href=@{adminRoute ShowR}>
                    Cancel, Oops..
                <br>
                <input type=submit value="Update Item.">
            |]
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
        [whamlet|
            Are you sure you want to delete this item: <br>
            <table .yesod-admin-listing>
                #{tableHeader table cols 0}
                <tr .yesod-admin-listing.values>
                    <td .yesod-admin-listing.values.key>
                        #{key}
                    #{tableRowValues item}
            <a href=@{adminRoute ShowR}>
                Cancel, Oops..
            <form method=post action=@{adminRoute (DeleteR table key)}>
                <input type=submit value="Yes, Delete.">
            |]

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
