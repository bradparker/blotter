module Blotter.Env
  ( blotterEnv
  , Env(..)
  , DatabaseEnv(..)
  ) where

import Blotter.Database
  (DatabaseEnv (..), createConnectionPool, openConnection)

data Env = Env
  { database :: DatabaseEnv
  }

blotterEnv :: IO Env
blotterEnv = do
  pool <- createConnectionPool
  return (Env (DatabaseEnv pool))
