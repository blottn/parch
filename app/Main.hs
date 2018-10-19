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
import Data.Foldable

archive = "archive"

pages = [("recents","/recents"),("login","/login"), ("new post","/post"), ("all","/all"), ("logout","/logout")]


index :: [H.Html] -> S.ActionM()
index recents = S.html . renderHtml $ do
          H.head $ H.title "Parch"
          H.body $ do
            H.h1 "Parch blog site"
            H.h2 "Contents"
            H.ul $ linesToHtml pages
            H.h2 "Recents"
            H.ul $ mapM_ (\x -> x) recents

listlink :: String -> AttributeValue -> H.Html
listlink x y = H.li $ H.a ! A.href y $ fromString x


linesToHtml :: [(String,AttributeValue)] -> H.Html
linesToHtml [(name,link)]= listlink name link
linesToHtml ((name,link):rest) = do
                              listlink name link
                              linesToHtml rest





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
              dir <- getCurrentDirectory
              time <- getModificationTime (dir </> archive </> x)
              return (x, time)

getPosts :: IO [(FilePath,UTCTime)]
getPosts = do
            dir <- getCurrentDirectory
            files <- listDirectory $ dir </> archive
            contents <- mapM conjoin files
            return $ sortBy (\(_,x) (_,y) -> compare x y) contents

getAllPosts :: IO [(FilePath, UTCTime)]
getAllPosts = getPosts

getRecentPosts :: IO [(FilePath, UTCTime)]
getRecentPosts = do
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
                        dir <- getCurrentDirectory
                        writeFile (dir </> archive </> title) content

renderPath :: FilePath -> H.Html
renderPath f = H.li $ H.a ! A.href (fromString ("/get/" ++ fromString ( takeFileName f))) $ fromString ( takeBaseName f)


main = S.scotty 3000 $ do
    S.get "/" $ do
      recents <- liftIO $ getRecentPosts
      paths <- mapM (\(f,_) -> return $ renderPath f) recents
      index paths

    S.get "/all" $ do
      posts <- liftIO getAllPosts
      S.html . renderHtml $ H.ul $ mapM_ (\(f,_) -> renderPath f) posts

   -- S.get "/recents" $ do
    --  posts <- getRecentPosts
    --  S.html . renderHtml $ H.ul $ mapM_ (\(f,_) -> renderPath f) posts

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
        dir <- getCurrentDirectory
        readFile (dir </> archive </> postid)
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
                            H.head $ H.title "Success!"
                            H.body $ do
                                H.h1 $ "Success!"
                                H.a ! A.href "/" $ "Home"
