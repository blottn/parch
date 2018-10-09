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

data Post = Post
    { id :: Int,
      time :: UTCTime,
      title :: String,
      content :: String
    } deriving (Read, Show)

getPost :: Int -> S.ActionM ()
getPost id = S.text (posts !! id)



posts = "hello world":"voila":[]

index :: S.ActionM()
index = S.html . renderHtml $ do
          H.head $
            H.title $ toHtml $ text "Parch"
          H.body $ do
            H.h1 $ toHtml $ text "Parch blog site"
            H.p $ toHtml $ text "here's a paragraph"
            
          

main = S.scotty 3000 $ do
    S.get "/get/:id" $ do
      id <- S.param "id"
      getPost id

    S.get "/" $ do
      index
   
