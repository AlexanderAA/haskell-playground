{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoMonomorphismRestriction      #-}

module PersistPG (
    main
) where

import           Control.Applicative
import           Data.Maybe
import qualified Control.Monad.IO.Class        as IOC
import qualified Data.ByteString.UTF8          as BU
import qualified Data.Text                     as T
import qualified Database.Persist              as P
import qualified Database.Persist.TH           as PT
import qualified Database.Persist.Postgresql   as PG
import qualified Database.Esqueleto            as E

-- | custom types are declared there
import qualified PersistK                      as PK 

-- | PostgreSQL connection string
pgConnString :: BU.ByteString
pgConnString = BU.fromString  $ unwords ["host=127.0.0.1 port=5432"
                                        ,"user=acid password=acidacid"])
                                        ,"dbname=acid"])

-- | Database schema
PT.share 
    [PT.mkPersist PT.sqlSettings, PT.mkMigrate "migrateAll"] 
    [PT.persistLowerCase|
        Person
            name PK.K
            age Int Maybe
            deriving Show
        Post
            title String
            authorId PersonId
            deriving Show
    |]

-- | Insert and select something
action = do
    postId <- E.insert (Person (PK.K "asz") (Just 28))
    posts <- E.select $ E.from $ \post -> do return post
    IOC.liftIO $ mapM_ (putStrLn . postTitle . E.entityVal) posts

-- | Execute
main :: IO ()
main = do
    PG.withPostgresqlPool pgConnString 5 (PG.runSqlPersistMPool action)
