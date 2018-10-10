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
          H.head $
            H.title $ toHtml $ text "Parch"
          H.body $ do
            H.h1 $ toHtml $ text "Parch blog site"
            H.p $ toHtml $ text "here's a paragraph"

main = S.scotty 3000 $ do
    S.get "/" $ do
      index
    S.get "/get/:id" $ do
      postid <- S.param "id"
      post <- liftIO $ do
        wd <- getCurrentDirectory
        post <- withCurrentDirectory (wd </> archive) $ readFile $ fromString postid
        return post
      S.html $ fromString post
