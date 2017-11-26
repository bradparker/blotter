module Blotter.Web.BlotterM
  ( BlotterM
  ) where

import Blotter.Trans (BlotterT)
import Blotter.Web.Error (Error)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT)

type BlotterM = ScottyT Error (BlotterT IO)
