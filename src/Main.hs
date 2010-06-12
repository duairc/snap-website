{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Control.Monad
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XML.Expat.Tree hiding (Node)

import Server

apiHandler :: TemplateState Snap -> Snap ()
apiHandler ts = do
    doc <- liftM rqPathInfo getRequest
    render (addSplices (splices doc) ts) "docs/api"
  where
    href doc     = B.concat ["/docs/latest/", doc, "/index.html"]
    docframe doc = return [mkElement "frame" [ ("id", "docframe")
                                             , ("src", href doc ) ] []]
    subtitle doc = return [mkText $ B.concat [": ", doc, " APIs"]]
    splices  doc = [("docframe", docframe doc), ("subtitle", subtitle doc)]


main :: IO ()
main = do
    config <- commandLineConfig
    templateServer "templates" splices config $ \ts ->
          route [("docs/api", apiHandler ts)]
      <|> ifTop (render ts "index")
      <|> templateServe ts
      <|> fileServe "static"
  where
    splices = [("server-version", return $ [Text snapServerVersion])]
