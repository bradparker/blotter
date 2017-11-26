module Blotter.Utils
  ( maybeToException
  ) where

import Control.Monad.Catch (Exception, MonadThrow, throwM)

maybeToException ::
     (Exception e, MonadThrow m) => e -> Maybe a -> m a
maybeToException _ (Just a) = return a
maybeToException e Nothing = throwM e
