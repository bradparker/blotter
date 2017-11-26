{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blotter.Articles.Database
  ( list
  , find
  , create
  , edit
  , delete
  , Error(..)
  ) where

import Blotter.Database (HasConnection, withConnection)
import Blotter.Utils (maybeToException)
import Control.Arrow (returnA)
import Control.Lens (set, view, _1, _3)
import Control.Monad.Catch (Exception, MonadThrow)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Profunctor.Product (p3)
import Data.Time (LocalTime, utc, utcToLocalTime)
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection)
import Opaleye
  ( Column
  , PGBool
  , PGTimestamp
  , PGUuid
  , Query
  , Table (Table)
  , constant
  , queryTable
  , required
  , restrict
  , runDelete
  , runInsertManyReturning
  , runQuery
  , runUpdateReturning
  , (.===)
  )

data Error
  = EmptyResult
  | Unknown String
  deriving (Show, Typeable)

instance Exception Error

type ArticleRow
   = (Column PGUuid, Column PGTimestamp, Column PGTimestamp)

type ArticleResult = (UUID, LocalTime, LocalTime)

articles :: Table ArticleRow ArticleRow
articles =
  Table
    "articles"
    (p3
       ( required "_id"
       , required "created_at"
       , required "updated_at"))

list :: HasConnection m => m [ArticleResult]
list = withConnection (flip runQuery (queryTable articles))

insert ::
     Connection -> (UUID, LocalTime) -> IO [ArticleResult]
insert conn (uuid, time) =
  runInsertManyReturning
    conn
    articles
    [constant (uuid, time, time)]
    id

create :: (MonadThrow m, HasConnection m) => m ArticleResult
create =
  withConnection $ \conn -> do
    uuid <- nextRandom
    time <- utcToLocalTime utc <$> getCurrentTime
    result <- listToMaybe <$> insert conn (uuid, time)
    maybeToException EmptyResult result

matchesUUID :: UUID -> ArticleRow -> Column PGBool
matchesUUID a = (constant a .===) . view _1

update ::
     Connection -> (UUID, LocalTime) -> IO [ArticleResult]
update conn (uuid, time) =
  runUpdateReturning
    conn
    articles
    (set _3 (constant time))
    (matchesUUID uuid)
    id

edit :: HasConnection m => UUID -> m (Maybe ArticleResult)
edit uuid =
  withConnection $ \conn -> do
    time <- utcToLocalTime utc <$> getCurrentTime
    listToMaybe <$> update conn (uuid, time)

delete :: HasConnection m => UUID -> m Int64
delete uuid =
  withConnection $ \conn ->
    runDelete conn articles (matchesUUID uuid)

queryByUUID :: UUID -> Query ArticleRow
queryByUUID uuidA =
  proc () ->
  do row@(uuidB, _, _) <- queryTable articles -< ()
     restrict -< constant uuidA .=== uuidB
     returnA -< row

find :: HasConnection m => UUID -> m (Maybe ArticleResult)
find uuid =
  withConnection $ \conn ->
    listToMaybe <$> runQuery conn (queryByUUID uuid)
