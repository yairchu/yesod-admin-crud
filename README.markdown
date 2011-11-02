Admin CRUD "sub-site" for the Yesod web framework.

CRUD means Create, Read, Update, Delete.
This module provides access to the site's database for an admin user.

## Setting up

Instructions of how to add the sub-site to Yesod's scaffolded website:

### Add routes

Add to `config/routes`

    /admin  AdminR  Admin  getAdmin

### Create separate module for "foundation type".

The site's foundation type needs to be moved out of `Foundation.hs` as it will be needed by `Model.hs`.
Not moving the type to separate module will cause an import cycle.

### Create the YesodAdmin instance

In `Model.hs`, change

    share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")

to

    share [mkPersist sqlSettings, mkMigrate "migrateAll", mkYesodAdmin ''SoundRadix] $(persistFile "config/models")

(Added `, mkYesodAdmin ''SoundRadix`)

### Create YesodAdminUser instance

In `Foundation.hs`, add

    instance YesodAdminUser SoundRadix where
        isAdminUser = return True

* You will probably want to change the code of `isAdminUser` to determine whether the logged-in user is an administrator user or not.
* `YesodAdminUser` is exported by `Yesod.Admin.User`.

### Fix imports

The changes describe above require several imports and language extensions. These should be simple to solve.

## TODO

* Make it look nicer.
* Selecting which part of the table to see if it has many rows. 
