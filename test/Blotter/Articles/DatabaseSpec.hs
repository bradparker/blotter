module Blotter.Articles.DatabaseSpec
  ( spec
  ) where

import Blotter.Articles.Database
  (create, delete, edit, find, list)
import Blotter.Test.Helpers.Database (runTestApp)
import Control.Monad (replicateM)
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  )

spec :: Spec
spec =
  describe "Blotter.Articles.Database" $ do
    describe "create" $
      it "creates an article" $ do
        (uuid, createdAt, updatedAt) <- runTestApp create
        createdAt `shouldBe` updatedAt
    describe "edit" $
      it "edits an article" $ do
        (Just (_, createdAt, updatedAt)) <-
          runTestApp $ do
            (uuid, _, _) <- create
            edit uuid
        createdAt `shouldNotBe` updatedAt
    describe "find" $
      it "finds an article" $ do
        (created, found) <-
          runTestApp $ do
            replicateM 2 create
            created@(id, _, _) <- create
            replicateM 2 create
            found <- find id
            return (created, found)
        found `shouldBe` Just created
    describe "delete" $
      it "deletes an article" $ do
        result <-
          runTestApp $ do
            (id, _, _) <- create
            delete id
            find id
        result `shouldBe` Nothing
    describe "list" $
      it "gets all articles" $ do
        (created, found) <-
          runTestApp $ do
            created <- replicateM 5 create
            found <- list
            return (created, found)
        found `shouldBe` created
