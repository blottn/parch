{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text.Lazy as T

find :: Int -> T.Text
find x | x == 1    = "hello world"
       | otherwise = "not found"

main = scotty 3000 $
  get "/get/:id" $ do
    id <- param "id"
    html $ find id
