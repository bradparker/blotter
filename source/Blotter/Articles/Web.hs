{-# LANGUAGE OverloadedStrings #-}

module Blotter.Articles.Web
  ( articles
  ) where

import Blotter.Articles.Database (create, edit, find, list)
import Blotter.Utils (maybeToException)
import Blotter.Web.ActionM (ActionM)
import Blotter.Web.BlotterM (BlotterM)
import Blotter.Web.Error (Error (InvalidParameter))
import Control.Monad.Catch (catch, throwM)
import Data.Text.Lazy (pack)
import Data.UUID (UUID, fromString)
import Web.Scotty.Trans
  (get, param, patch, post, raise, text)

handleError :: Error -> ActionM ()
handleError e = text $ pack $ show e

idParam :: ActionM UUID
idParam =
  do id <- param "id"
     maybeToException
       (InvalidParameter ("uuid", id))
       (fromString id)
     `catch` raise

articlesCreate :: ActionM ()
articlesCreate = do
  article <- create
  text $ pack $ show article

articlesEdit :: ActionM ()
articlesEdit = do
  id <- idParam
  article <- edit id
  text $ pack $ show article

articlesIndex :: ActionM ()
articlesIndex = do
  articles <- list
  text $ pack $ show articles

articlesShow :: ActionM ()
articlesShow = do
  id <- idParam
  article <- find id
  text $ pack $ show article

articles :: BlotterM ()
articles = do
  post "/articles" articlesCreate
  get "/articles" articlesIndex
  patch "/articles/:id" articlesEdit
  get "/articles/:id" articlesShow
