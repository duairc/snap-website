{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Control.Monad
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XML.Expat.Tree hiding (Node)

import           Glue
import           Server

apiHandler :: TemplateState Snap -> Snap ()
apiHandler ts = do
    doc <- liftM rqPathInfo getRequest
    render (bindSplices (splices doc) ts) "docs/api"
  where
    href doc     = B.concat ["/docs/latest/", doc, "/index.html"]
    docframe doc = return [mkElement "frame" [ ("id", "docframe")
                                             , ("src", href doc ) ] []]
    subtitle doc = return [mkText $ B.concat [": ", doc, " APIs"]]
    splices  doc = [("docframe", docframe doc), ("subtitle", subtitle doc)]


main :: IO ()
main = do
    td <- newTemplateDirectory' "templates" (withSplices splices)
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
          route [("docs/api", apiHandler ts)]
      <|> templateServe ts
      <|> fileServe "static"
  where
    splices = [("server-version", return $ [Text snapServerVersion])]
