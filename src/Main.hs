{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8(ByteString)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XML.Expat.Tree hiding (Node)

import Server

whichApiDoc :: TemplateMonad Snap ByteString
whichApiDoc = lift $ liftM rqPathInfo getRequest

serverVersion :: Splice Snap
serverVersion = return $ [Text snapServerVersion]

docframe :: Splice Snap
docframe = do
    doc <- whichApiDoc
    let href = B.concat ["/docs/latest/", doc, "/index.html"]
    return [mkElement "frame" [ ("id", "docframe")
                              , ("src", href     ) ] [] ]
apiSubtitle :: Splice Snap
apiSubtitle = do
    doc <- whichApiDoc
    return [mkText $ B.concat [": ", doc, " APIs"]]

splices :: [(ByteString, Splice Snap)]
splices = [ ("server-version", serverVersion)
          , ("docframe",       docframe     )
          , ("api-subtitle",   apiSubtitle  )
          ]

main :: IO ()
main = templateServer "templates" splices emptyServerConfig $ \render ->
           route [("docs/api", render "docs/api")]
       <|> ifTop (render "index")
       <|> (render . B.pack =<< getSafePath)
       <|> fileServe "static"
