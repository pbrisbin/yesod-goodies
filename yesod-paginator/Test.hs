{-# LANGUAGE CPP                   #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
module Test where

import Yesod.Paginator

import Yesod
import Yesod.Persist
import Network.Wai.Handler.Warp (run)
import Database.Persist.Base
import Database.Persist.Sqlite
import Database.Persist.GenericSql
import Control.Monad
import Text.Blaze (toHtml)

-- testing selectPaginated, comment to test paginate
#define DB

share [mkPersist sqlSettings, mkMigrate "migrateThings"]
    [persist|
        Thing
            title String
            descr String

            UniqueThing title
         |]

data PaginatorTest = PaginatorTest { connPool :: ConnectionPool }

mkYesod "PaginatorTest" [parseRoutes| 
    / RootR GET
    |]

instance Yesod PaginatorTest where approot _ = ""

instance YesodPersist PaginatorTest where
    type YesodPersistBackend PaginatorTest = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= runPool (undefined :: SqliteConf) f

withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "things.db3" 10

getRootR :: Handler RepHtml
getRootR = do
    setupThings

#ifdef DB
    (things, widget) <- selectPaginated 10 [] []
#else
    things' <- runDB $ selectList [] []
    (things,widget) <- paginate 10 things'
#endif

    defaultLayout $ do
        setTitle "paginator test page"


        [whamlet|
            <h1>Test Page

            <ul>
            $forall thing <- things
                <li>
                    #{thingTitle $ snd thing} - 
                    #{thingDescr $ snd thing}

            ^{widget}
            |]

setupThings :: Handler ()
setupThings = do
    forM (map show [1..1040]) $ \n -> do
        let thing = Thing ("thing_" ++ n) ("This is thing #" ++ n)
        _ <- runDB $ insertBy thing
        return ()

    return ()

main :: IO ()
main = putStrLn "Loaded" >> withPaginatorTest (run 3000)

withPaginatorTest :: (Application -> IO a) -> IO a
withPaginatorTest f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migrateThings) p
    let h = PaginatorTest p
    toWaiApp h >>= f
