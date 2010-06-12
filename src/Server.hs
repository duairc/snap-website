{-# LANGUAGE OverloadedStrings #-}
module Server
    ( server
    , AppConfig(..)
    , emptyServerConfig
    , commandLineConfig

    , templateServer
    , templateServe
    , render
    , addSplices
    ) where
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           System
import           System.Posix.Env
import           Text.Templating.Heist
import qualified Text.XHtmlCombinators.Escape as XH

import           Templates

data AppConfig = AppConfig
    { locale             :: String
    , interface          :: ByteString
    , port               :: Int
    , hostname           :: ByteString
    , accessLog          :: Maybe FilePath
    , errorLog           :: Maybe FilePath
    , compression        :: Bool
    , error500Handler    :: SomeException -> Snap ()
    }

emptyServerConfig :: AppConfig
emptyServerConfig = AppConfig
    { locale          = "en_US"
    , interface       = "*"
    , port            = 8000
    , hostname        = "myserver"
    , accessLog       = Just "access.log"
    , errorLog        = Just "error.log"
    , compression     = True
    , error500Handler = \e -> do
        let t = T.pack $ show e
            r = setContentType "text/html; charset=utf-8" $
                setResponseStatus 500 "Internal Server Error" emptyResponse
        putResponse r
        writeBS "<html><head><title>Internal Server Error</title></head>"
        writeBS "<body><h1>Internal Server Error</h1>"
        writeBS "<p>A web handler threw an exception. Details:</p>"
        writeBS "<pre>\n"
        writeText $ XH.escape t
        writeBS "\n</pre></body></html>"
    }

commandLineConfig :: IO AppConfig
commandLineConfig = do
    args <- getArgs
    return $ case args of
         []        -> emptyServerConfig
         (port':_) -> emptyServerConfig { port = read port' }

server :: AppConfig -> Snap () -> IO ()
server config handler = do
    putStrLn $ "Listening on " ++ (B.unpack $ interface config)
             ++ ":" ++ show (port config)
    setUTF8Locale (locale config)
    try $ httpServe
             (interface config)
             (port      config)
             (hostname  config)
             (accessLog config)
             (errorLog  config)
             (catch500 $ compress $ handler)
             :: IO (Either SomeException ())
    threadDelay 1000000
    putStrLn "Shutting down"
  where
    catch500 = (`catch` (error500Handler config))
    compress = if compression config then withCompression else id


templateServer :: FilePath
               -> [(ByteString, Splice Snap)]
               -> AppConfig
               -> (TemplateState Snap -> Snap ())
               -> IO ()
templateServer dir' splices config f = do
    eT <- newTemplates dir' splices
    t <- fromRight eT
    server config $ reloadHandler t <|> (f =<< getTs t)
  where
    fromRight = either (\s -> putStrLn s >> exitFailure) return


reloadHandler :: Templates Snap -> Snap ()
reloadHandler t = path "admin/reload" $ do
    e <- refresh t
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS . B.pack $ either id (const "Templates loaded successfully.") e


render :: TemplateState Snap -> ByteString -> Snap ()
render ts template = do
    bytes <- renderTemplate ts template
    flip (maybe pass) bytes $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS x


templateServe :: TemplateState Snap -> Snap ()
templateServe ts = render ts . B.pack =<< getSafePath


setUTF8Locale :: String -> IO ()
setUTF8Locale locale' = do
    mapM_ (\k -> setEnv k (locale' ++ ".UTF-8") True)
          [ "LANG"
          , "LC_CTYPE"
          , "LC_NUMERIC"
          , "LC_TIME"
          , "LC_COLLATE"
          , "LC_MONETARY"
          , "LC_MESSAGES"
          , "LC_PAPER"
          , "LC_NAME"
          , "LC_ADDRESS"
          , "LC_TELEPHONE"
          , "LC_MEASUREMENT"
          , "LC_IDENTIFICATION"
          , "LC_ALL" ]
