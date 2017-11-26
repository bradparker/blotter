{-# LANGUAGE FlexibleInstances #-}

module Blotter.Web.ActionM
  ( ActionM
  ) where

import Blotter.Database (HasConnection, withConnection)
import Blotter.Env (DatabaseEnv (..), Env (..))
import Blotter.Trans (BlotterT)
import Blotter.Web.Error (Error)
import Control.Monad.Catch
  (MonadCatch (..), MonadThrow (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, lift)
import Data.Pool (withResource)
import Web.Scotty.Internal.Types (ActionT (..))

type ActionM = ActionT Error (BlotterT IO)

instance HasConnection ActionM where
  withConnection action = do
    pool <- lift (asks (connections . database))
    liftIO (withResource pool action)

instance MonadThrow ActionM where
  throwM = ActionT . throwM

instance MonadCatch ActionM where
  catch (ActionT m) f = ActionT (m `catch` (runAM . f))
