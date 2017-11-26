{-# LANGUAGE MultiParamTypeClasses #-}

module Blotter.Database
  ( HasConnection
  , withConnection
  , openConnection
  , createConnectionPool
  , DatabaseEnv(..)
  ) where

import Control.Monad.Reader (MonadIO, MonadReader)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
  (Connection, close, connectPostgreSQL)
import System.Environment (getEnv)

data DatabaseEnv = DatabaseEnv
  { connections :: Pool Connection
  }

class HasConnection m where
  withConnection :: (Connection -> IO b) -> m b

openConnection :: IO Connection
openConnection =
  connectPostgreSQL =<< pack <$> getEnv "DATABASE_URL"

createConnectionPool :: IO (Pool Connection)
createConnectionPool =
  createPool openConnection close 1 10 8
