{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty


main = scotty 3000 $
  get "/" $ do
    html $ "hello world"
