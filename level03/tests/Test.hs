{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import           Test.Hspec.Wai

-- import           Data.String                (fromString)
-- import qualified System.Exit                as Exit
-- import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified FirstApp.Main              as Main

main :: IO ()
main = do
  -- We need to setup our Application.
  let app' = pure Main.app

  -- This sets up HSpec to use our application as the thing it executes before the tests are run
  hspec . with app' $ do
      -- Here is an example test for the 'ListRq' route.
      -- Start with a general description of what we're going to test.
      describe "List Route" $ do
        -- Individual test cases provide more precise information regarding
        -- what they are going to test.
        it "Should return a 'not implemented' message and 200 status" $
          -- Using the functions from ``Test.Hspec.Wai`` this actions a GET request
          -- on the "/list" route, and using an infix function, compares the result of
          -- that request to our expected result.

          -- There String literal here is being converted by the use of the
          -- ``IsString`` typeclass into a response type that Hspec.Wai can
          -- use. Check the documentation for more examples, but when given
          -- a string literal, it will assume that is the expected body of
          -- the request and also check for a 200 response code.
          get "/list" `shouldRespondWith` "List Request not implemented"

      -- Write some more tests, below are some ideas to get you started:
      -- Don't worry if you don't get all of these done. :)
      describe "Add Route" $ do
      -- 1) The '<topic>/add' route will respond with an error when given an empty comment
        it "should respond with error when given an empty comment" $
          post "/puppy/add" "" `shouldRespondWith` "Empty Comment" { matchStatus = 400 }
      -- 5) The '<topic>/add' route will respond with the message from the config (the `mkMessage` function from FirstApp.Conf will help)
        it "should add new comment" $
          post "/puppy/add" "aww" `shouldRespondWith` "Hello there!"


      describe "View Route" $ do
      -- 2) The '<topic>/view' route will respond correctly when given a topic
        it "should respond for a given topic" $
          get "/puppy/view" `shouldRespondWith` "View Request not implemented"
      -- 3) The '<topic>/view' route will respond with an error when given an empty topic
        it "should respond with error when given empty topic" $
          get "//view" `shouldRespondWith` "Empty Topic" { matchStatus = 400 }

      -- 4) A gibberish route will return a 404
      describe "Invalid route" $ do
        it "should respond with an error for a gibberish route" $
          get "/howdy1234" `shouldRespondWith` "Unknown Route" { matchStatus = 404 }
        it "should respond with an error when using GET with add route" $
          get "/puppy/add" `shouldRespondWith` "Unknown Route" { matchStatus = 404 }


