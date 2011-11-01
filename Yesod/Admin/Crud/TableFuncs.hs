{-# LANGUAGE FlexibleContexts #-}

module Yesod.Admin.Crud.TableFuncs (TableFuncs(..), tableFuncs) where

import Yesod.Admin.Crud.Type (YesodAdminReqs)

import Data.Maybe (fromJust)
import Data.String (IsString(fromString))
import Data.Text (Text)

import Database.Persist.Base (
    EntityDef(..), ColumnDef(columnName), Key, PersistBackend, PersistEntity(..), PersistValue(..), SelectOpt(LimitTo), SomePersistField(..),
    delete, entityDef, get, insert, replace, selectList, toPersistValue)
import Yesod (GHandler, SinglePiece(..), YesodPersist(..), lookupPostParam, runDB)

data TableFuncs sub master = TableFuncs
    { getRow :: Text -> GHandler sub master [SomePersistField]
    , listRows :: GHandler sub master [(PersistValue, [SomePersistField])]
    , addRow :: GHandler sub master ()
    , editRow :: Text -> GHandler sub master ()
    , deleteRow :: Text -> GHandler sub master ()
    , tableDef :: EntityDef
    }

tableFuncs ::
    ( YesodAdminReqs sub master, PersistEntity v, SinglePiece (Key (YesodPersistBackend master) v)
    ) =>
    v -> GHandler sub master (TableFuncs sub master)
tableFuncs dummy =
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

