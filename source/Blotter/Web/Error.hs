module Blotter.Web.Error
  ( Error(..)
  ) where

import qualified Blotter.Articles.Database as ADB
import           Control.Monad.Catch (Exception)
import           Data.String (fromString)
import           Data.Typeable (Typeable)
import           Web.Scotty.Trans (ScottyError (..))

data Error
  = InvalidParameter (String, String)
  | Unknown String
  deriving (Show, Typeable)

instance Exception Error

instance ScottyError Error where
  stringError = Unknown
  showError = fromString . show

instance ScottyError ADB.Error where
  stringError = ADB.Unknown
  showError = fromString . show
