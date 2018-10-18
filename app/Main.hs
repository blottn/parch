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
          H.head $ H.title "Parch"
          H.body $ do
            H.h1 "Parch blog site"
            H.h2 "Contents"
            H.ul $ linesToHtml pages

listlink :: String -> AttributeValue -> H.Html
listlink x y = H.li $ H.a ! A.href y $ fromString x


linesToHtml :: [(String,AttributeValue)] -> H.Html
linesToHtml [(x,y)]= listlink x y
linesToHtml ((x,y):rest) = do
                              listlink x y
                              linesToHtml rest


renderPath :: FilePath -> H.Html
renderPath f = H.li $ H.a ! A.href (fromString ("/get/" ++ fromString ( takeFileName f))) $ fromString ( takeBaseName f)


loginPage :: Maybe Text -> S.ActionM()
loginPage (Just "y") = S.html . renderHtml $ do
                              H.head $ H.title "Parch - login"
                              H.body $ do
                                H.p "Already logged in!"
                                H.a ! A.href "/" $ "home"

loginPage Nothing = S.html . renderHtml $ do
                            H.head $ H.title "Parch - login"
                            H.body $ do
                              H.a ! A.href "/" $ "home"
                              H.form ! A.action "/auth" ! A.method "post" $ do
                                H.input ! A.type_ "text" ! A.placeholder "username" ! A.name "name"
                                H.br
                                H.input ! A.type_ "password" ! A.placeholder "password" ! A.name "pass"
                                H.br
                                H.input ! A.type_ "submit"


auth :: Text -> Text -> S.ActionM()
auth "admin" "pass" = do
                        C.setSimpleCookie "auth" "y"
                        S.redirect "/success"
auth _ _ = S.text "incorrect"


conjoin :: FilePath -> IO (FilePath, UTCTime)
conjoin x = do
              wd <- getCurrentDirectory
              time <- withCurrentDirectory (wd </> archive) $ getModificationTime x
              return (x, time)

getPosts :: IO [(FilePath,UTCTime)]
getPosts = do
            dir <- getCurrentDirectory
            contents <- listDirectory $ dir </> archive
            mapM conjoin contents

getAllPosts :: IO [(FilePath, UTCTime)]
getAllPosts = do
                files <- getPosts
                return $ sortBy (\(_,x) (_,y) -> compare x y) files

getRecentPosts :: S.ActionM [(FilePath, UTCTime)]
getRecentPosts = liftIO  $ do
                            files <- getPosts
                            return $ take 5 $ sortBy (\(_,x) (_,y) -> compare y x) files

postPage :: Maybe Text -> S.ActionM ()
postPage Nothing = S.redirect "/"
postPage (Just "y") = S.html . renderHtml $ do
                                            H.head $ H.title "Parch - Create Post"
                                            H.body $ do
                                              H.h1 "Create Post"
                                              H.form ! A.action "/create" ! A.method "post" $ do
                                                H.input ! A.type_ "text" ! A.name "title" ! A.placeholder "title"
                                                H.br
                                                H.textarea ! A.cols "40" ! A.rows "6" ! A.name "content" $ ""
                                                H.input ! A.type_ "submit"

mkpost :: String -> String -> IO ()
mkpost title content = do
                        wd <- getCurrentDirectory
                        writeFile (wd </> archive </> title) content

main = S.scotty 3000 $ do
    S.get "/" index

    S.get "/all" $ do
      posts <- liftIO getAllPosts
      S.html . renderHtml $ H.ul $ mapM_ (\(f,_) -> renderPath f) posts

    S.get "/recents" $ do
      posts <- getRecentPosts
      S.html . renderHtml $ H.ul $ mapM_ (\(f,_) -> renderPath f) posts

    S.get "/login" $ do
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
        withCurrentDirectory (wd </> archive) $ readFile $ fromString postid
      S.text $ fromString post

    S.get "/post" $ do
      authed <- C.getCookie "auth"
      postPage authed

    S.post "/auth" $ do
      name <- S.param "name"
      pass <- S.param "pass"
      auth name pass

    S.post "/create" $ do
      title <- S.param "title"
      content <- S.param "content"
      liftIO $ mkpost title content
      S.html "posted"

    S.get "/success" $ S.html . renderHtml $ do
                            H.head $ H.title "You successfully did something!"
                            H.body $ H.a ! A.href "/" $ "Home"
