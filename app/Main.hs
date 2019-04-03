{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import Control.Monad (forever)
import Network.Wreq (defaults, get, responseBody, responseHeaders)
import Network.Wreq.Types (checkResponse, Options)
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, attribute, element, fromDocument, child,
                        ($//), (&|), (&//), (>=>))


opts :: Options
opts = defaults {checkResponse = Just $ \_ _ -> return ()}

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

main :: IO ()
main = getRandomUrl "en" >>= print
