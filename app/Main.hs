{-# LANGUAGE OverloadedStrings #-}
module Main( main) where

import Control.Monad.IO.Class
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as C
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
import Data.List

-- Data types:

archive = "archive"

pages = [("Blog!","/get/blog"),("recents","/recents"),("login","/login"), ("new post","/post"), ("all","/all"), ("logout","/logout")]


index :: S.ActionM()
index = S.html . renderHtml $ do
          H.head $ do
            H.title "Parch"
          H.body $ do
            H.h1 "Parch blog site"
            H.h2 "Contents"
            H.ul $ do
                linesToHtml pages
listlink :: String -> AttributeValue -> H.Html
listlink x y = H.li $ H.a ! A.href y $ fromString x


linesToHtml :: [(String,AttributeValue)] -> H.Html
linesToHtml ((x,y):[])= listlink x y
linesToHtml ((x,y):rest) = do
                              listlink x y
                              linesToHtml rest


renderPath :: FilePath -> H.Html
renderPath f = H.li $ H.a ! A.href (fromString ("/get/" ++ (fromString $ takeFileName f))) $ (fromString $ takeBaseName f)


loginPage :: (Maybe Text) -> S.ActionM()
loginPage (Just "y") = S.html . renderHtml $ do
                              H.head $ do
                                H.title "Parch - login"
                              H.body $ do
                                H.p "Already logged in!"
                                H.a ! A.href "/" $ "home"

loginPage Nothing = S.html . renderHtml $ do
                            H.head $ do
                              H.title "Parch - login"
                            H.body $ do
                              H.a ! A.href "/" $ "home"
                              H.form ! A.action "/auth" ! A.method "post" $ do
                                H.input ! A.type_ "text" ! A.name "name"
                                H.input ! A.type_ "password" ! A.name "pass"
                                H.input ! A.type_ "submit"


auth :: Text -> Text -> S.ActionM()
auth "admin" "pass" = do
                        C.setSimpleCookie "auth" "y"
                        S.redirect "/success"
auth _ _ = S.text "oh no"


conjoin :: FilePath -> IO((FilePath, UTCTime))
conjoin x = do
              wd <- getCurrentDirectory
              time <- withCurrentDirectory (wd </> archive) $ getModificationTime x
              return (x, time)

getAllPosts :: IO ([(FilePath, UTCTime)])
getAllPosts = do
                dir <- getCurrentDirectory
                contents <- listDirectory $ dir </> archive
                files <- mapM conjoin contents
                return $ sortBy (\(_,x) (_,y) -> compare x y) files

main = S.scotty 3000 $ do
    S.get "/" $ do
      index

    S.get "/all" $ do
      files <- liftIO $ getAllPosts
      S.html . renderHtml $ H.ul $ mapM_ (\(f,_) -> renderPath f) files

    S.get "/recents" $ do
      files <- liftIO $ getAllPosts
      S.html "TODO"

    S.get "/login" $ do
      --debug <- S.param "debug"
      auth <- C.getCookie "auth"
      loginPage auth

    S.get "/logout" $ do
      C.deleteCookie "auth"
      S.html . renderHtml $ do
                            H.head $ H.title "logged out"
                            H.body $ do
                              H.p "Successfully logged out"
                              H.a ! A.href "/" $ "Home"

    S.get "/get/:id" $ do
      postid <- S.param "id"
      post <- liftIO $ do
        wd <- getCurrentDirectory
        post <- withCurrentDirectory (wd </> archive) $ readFile $ fromString postid
        return post
      S.text $ fromString post

    S.post "/auth" $ do
      name <- S.param "name"
      pass <- S.param "pass"
      auth name pass

    S.get "/success" $ do
      S.html . renderHtml $ do
                            H.head $ H.title "You successfully did something!"
                            H.body $ H.a ! A.href "/" $ "Home"
