{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blotter.Trans
  ( BlotterT
  , runBlotterT
  ) where

import Blotter.Env (Env)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  (MonadReader, ReaderT, runReaderT)

newtype BlotterT m a = BlotterT
  { unBlotterT :: ReaderT Env m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadReader Env
             )

runBlotterT :: Env -> BlotterT m a -> m a
runBlotterT e m = runReaderT (unBlotterT m) e
