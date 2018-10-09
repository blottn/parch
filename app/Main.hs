{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html5 as H

getPost :: Int -> T.Text
getPost x = posts !! x

posts = "hello world":"voila":[]



index :: ActionM()
index = html "<h1> Welcome to parch! </h1>"

main = scotty 3000 $ do
    get "/get/:id" $ do
      id <- param "id"
      text $ getPost id

    get "/" $ index
