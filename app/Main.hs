{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as T

find :: Int -> T.Text
find x | x == 1    = "hello world"
       | otherwise = "not found"

index :: ActionM()
index = html "<h1> Welcome to parch! </h1>"

main = scotty 3000 $ do
    get "/get/:id" $ do
      id <- param "id"
      text $ find id

    get "/" $ index
