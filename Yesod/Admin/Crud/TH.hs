{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Yesod.Admin.Crud.TH (mkYesodAdmin) where

import Language.Haskell.TH.Syntax (Body(..), Clause(..), Dec(..), Exp(..), Lit(..), Pat(..), Pred(..), Q, Type(..), mkName)

import Database.Persist.Base (EntityDef(..), Key)
import Yesod (SinglePiece, YesodPersistBackend)

import Yesod.Admin.Crud.Class (YesodAdmin(..))
import Yesod.Admin.Crud.TableFuncs (tableFuncs)
import Yesod.Admin.Crud.Type (Admin, YesodAdminReqs)

mkYesodAdmin :: [EntityDef] -> Q [Dec]
mkYesodAdmin defs =
    return [ InstanceD context (AppT (ConT ''YesodAdmin) master) body ]
    where
        context = ClassP ''YesodAdminReqs [ConT ''Admin, master] : map singlePiece defs
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
        getTableFuncsBody def = AppE (VarE 'tableFuncs) . SigE (VarE 'undefined) . ConT . mkName $ entityName def
