{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Control.Monad.IO.Class
import qualified Web.Scotty as S
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String
import Data.Text (unpack, pack, Text)
import qualified Data.Text.IO as T
import System.FilePath
import System.Directory
import Data.Time

-- Data types:

archive = "archive"

index :: S.ActionM()
index = S.html . renderHtml $ do
          H.head $ do
            H.title "Parch"
          H.body $ do
            H.h1 "Parch blog site"
            H.h2 "Contents"
            H.ul $ do
                H.li $ H.a ! A.href "/get/blog" $ "my link"
                H.li $ "another asdf"

loginPage :: Bool -> S.ActionM()
loginPage True = S.html . renderHtml $ do
                              H.head $ do
                                H.title "Parch - login"
                              H.body $ do
                                H.p "Already logged in!"
                                H.a ! A.href "/" $ "home"
                                
loginPage False = S.html "not logged in"

auth :: Text -> Text -> S.ActionM()
auth "admin" "pass" = do 
                        S.text "success"
auth _ _ = S.text "oh no"

main = S.scotty 3000 $ do
    S.get "/" $ do
      index

    S.get "/recents" $ do
      S.html "TODO"

    S.get "/login/:debug" $ do
      debug <- S.param "debug"
      loginPage debug

    S.get "/get/:id" $ do
      postid <- S.param "id"
      post <- liftIO $ do
        wd <- getCurrentDirectory
        post <- withCurrentDirectory (wd </> archive) $ readFile $ fromString postid
        return post
      S.html $ fromString post

    S.get "/auth/:name/:pass" $ do
      name <- S.param "name"
      pass <- S.param "pass"
      auth name pass

