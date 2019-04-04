{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import Network.Wreq (defaults, get, postWith, responseBody, responseStatus, statusCode)
import Network.Wreq.Types (checkResponse, Options, auth, Auth(OAuth1))
import Network.Wreq.Lens (param)
import Control.Lens ((^.), (&), (.~))
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, attribute, element, fromDocument, ($//), (&|), (>=>))
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad (forever)

opts :: Options
opts = defaults {checkResponse = Just $ \_ _ -> return ()}


-- Wikipedia API
getRandomArticle :: String -> IO BSL.ByteString
getRandomArticle lang = do
  response <- get $ "http://" ++ lang ++ ".wikipedia.org/wiki/Special:Random"
  return $ response ^. responseBody

getUrlFromPage :: BSL.ByteString -> String
getUrlFromPage html = let cursor = fromDocument $ parseLBS html
                      in processData $ cursor $// (pageUrlNode &| cursorContent)

pageUrlNode :: Cursor -> [Cursor]
pageUrlNode = element "link" >=> attributeIs "rel" "canonical"

cursorContent :: Cursor -> T.Text
cursorContent = T.concat . (attribute "href")

processData :: [T.Text] -> String
processData =  T.unpack . T.concat

getRandomUrl :: String -> IO String
getRandomUrl lang = getUrlFromPage <$> getRandomArticle lang



-- Twitter API
secretPath :: String
secretPath = "./secret"
emptyBody :: Aeson.Value
emptyBody = Aeson.toJSON (Nothing :: Maybe String)

getOAuthCreds :: FilePath -> ExceptT String IO Auth
getOAuthCreds path = do
  c <- liftIO $ Aeson.decodeStrict <$> BS.readFile path
  let cred c n = S8.pack $ c !! n
  case (c :: Maybe [String]) of
    Nothing -> throwE ""
    (Just c) -> return $ OAuth1 (cred c 0) (cred c 1) (cred c 2) (cred c 3)

verify r msg =
  let code = r ^. responseStatus ^. statusCode
  in if code == 200 then return () else throwE  ""

tweet :: String -> Auth -> ExceptT String IO ()
tweet text authCreds = do
  response <- liftIO $ postWith
              (opts {auth = Just authCreds} & param "status".~ [T.pack text])
              "https://api.twitter.com/1.1/statuses/update.json"
              emptyBody
  verify response $ "Posting: " ++ text



main :: IO ()
main = getRandomUrl "en" >>= print
