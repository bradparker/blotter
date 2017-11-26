{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blotter.Test.Helpers.Database
  ( runTestApp
  ) where

import Blotter.Database
  (HasConnection, openConnection, withConnection)
import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
  (MonadReader, ReaderT, ask, runReaderT)
import Database.PostgreSQL.Simple
  (Connection, begin, close, rollback)

instance HasConnection TestAppM where
  withConnection action = do
    conn <- ask
    liftIO (action conn)

newtype TestAppM a = TestAppM
  { unTestAppM :: ReaderT Connection IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadReader Connection
             )

runTestApp :: TestAppM a -> IO a
runTestApp action = do
  conn <- openConnection
  bracket
    (begin conn *> return conn)
    (\conn -> rollback conn *> close conn)
    (runReaderT (unTestAppM action))
