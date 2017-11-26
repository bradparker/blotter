{-# LANGUAGE OverloadedStrings #-}

module Blotter.Web where

import Blotter.Articles.Web (articles)
import Blotter.Env (blotterEnv)
import Blotter.Trans (runBlotterT)
import Blotter.Web.ActionM (ActionM)
import Blotter.Web.BlotterM (BlotterM)
import Blotter.Web.Error (Error (..))
import Data.String (fromString)
import Network.HTTP.Types (Status, mkStatus, status500)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty.Trans
  ( Options (..)
  , defaultHandler
  , middleware
  , scottyOptsT
  , status
  , text
  )

status422 :: Status
status422 = mkStatus 422 "Unprocessable entity"

options :: Options
options = Options {verbose = 1, settings = defaultSettings}

handleError :: Error -> ActionM ()
handleError (InvalidParameter (k, v)) = do
  status status422
  text $ fromString $ "Invalid parameter " ++ k ++ v
handleError _ = do
  status status500
  text $ fromString $ "A thing, went wrong"

runBlotter :: IO ()
runBlotter = do
  e <- blotterEnv
  scottyOptsT options (runBlotterT e) app

app :: BlotterM ()
app = do
  middleware logStdoutDev
  defaultHandler handleError
  articles
