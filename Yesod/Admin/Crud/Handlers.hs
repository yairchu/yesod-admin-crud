{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Yesod.Admin.Crud.Handlers where

import Control.Monad (unless)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax (Pred(ClassP), Type(VarT), mkName)

import Database.Persist.Base (EntityDef(..), PersistField, PersistValue(PersistNull), SomePersistField, columnName, toPersistValue)
import Text.Cassius (cassiusFile)
import Text.Hamlet (shamletFile)
import Yesod (
    GHandler, GWidget, Html, RedirectType(RedirectTemporary), RenderRoute(..), RepHtml, Yesod, YesodDispatch(..),
    addCassius, defaultLayout, getRouteToMaster, mkYesodSub, permissionDenied, parseRoutes, redirect, toSinglePiece, whamletFile)

import Yesod.Admin.Crud.Class (YesodAdmin(..))
import Yesod.Admin.Crud.TableFuncs (TableFuncs(..))
import Yesod.Admin.Crud.Type (Admin)
import Yesod.Admin.User (isAdminUser)

mkYesodSub "Admin" [ClassP ''YesodAdmin [VarT (mkName "master")]] [parseRoutes|
/ ShowR GET
/add/#String AddR POST
/edit/#String/#Text EditR GET POST
/delete/#String/#Text DeleteR GET POST
|]

checkAdmin :: YesodAdmin master => GHandler Admin master ()
checkAdmin = do
    isAdmin <- isAdminUser
    unless isAdmin $ permissionDenied mempty

getShowH :: YesodAdmin master => GHandler Admin master [(String, [String], [(PersistValue, [SomePersistField])])]
getShowH = do
    checkAdmin
    getTableNames >>= mapM tableData
    where
        tableData table = do
            tableFuncs <- getTableFuncs table
            rows <- listRows tableFuncs
            let cols = map columnName . entityColumns $ tableDef tableFuncs
            return (table, cols, rows)

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
    checkAdmin
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
    checkAdmin
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
    checkAdmin
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
    checkAdmin
    tableFuncs <- getTableFuncs table
    addRow tableFuncs
    directHome

postEditR :: YesodAdmin master => String -> Text -> GHandler Admin master ()
postEditR table key = do
    checkAdmin
    tableFuncs <- getTableFuncs table
    editRow tableFuncs key
    directHome

postDeleteR :: YesodAdmin master => String -> Text -> GHandler Admin master ()
postDeleteR table key = do
    checkAdmin
    tableFuncs <- getTableFuncs table
    deleteRow tableFuncs key
    directHome
