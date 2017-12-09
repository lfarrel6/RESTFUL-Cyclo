{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Db (runDatabase) where

import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Define our entities as usual
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Commit
    repoId RepoId
    refCode String
    complexity Int
    nodeCount Int
    started UTCTime  default=CURRENT_TIME
    finished UTCTime Maybe
    UniqueRef refCode
    deriving Show
Repo
    name String
    url String
    UniqueUrl url
    deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data PersistTest = PersistTest ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "PersistTest" [parseRoutes|
/ HomeR GET
/repo/#RepoId RepoR GET
/commit/#CommitId CommitR GET
|]

-- Nothing special here
instance Yesod PersistTest

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist PersistTest where
    type YesodPersistBackend PersistTest = SqlBackend

    runDB action = do
        PersistTest pool <- getYesod
        runSqlPool action pool

-- List all people in the database
getHomeR :: Handler Html
getHomeR = do
    repos <- runDB $ selectList [] []
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity repoId repo <- repos
                    <li>
                        <a href=@{RepoR repoId}>#{repoName repo}
        |]

getRepoR :: RepoId -> Handler Html
getRepoR rId = do
    commits <- runDB $ selectList [CommitRepoId ==. rId] []
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity commitId commit <- commits
                    <li>
                        <a href=@{CommitR commitId}>#{commitRefCode commit}
        |]

-- We'll just return the show value of a person, or a 404 if the Person doesn't
-- exist.
getCommitR :: CommitId -> Handler String
getCommitR commitId = do
    commit <- runDB $ get404 commitId
    return $ show commit

openConnectionCount :: Int
openConnectionCount = 10

runDatabase :: IO ()
runDatabase = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        repoId <- insert $ Repo "Restful Cyclo" "https://github.com/lfarrel6/RESTFUL-Cyclo"
        now    <- liftIO $ getCurrentTime
        insert $ Commit repoId "244c4d43cac1237e1c01032d34ce845d336fc52e" 0 2  now Nothing
    warp 3000 $ PersistTest pool