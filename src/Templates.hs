module Templates(newTemplates,renderTemplates,reloadTemplates,Splices,Templates()) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


type Splices m = [(ByteString, Splice m)]


data Templates m = Templates
    { templateDir' :: FilePath
    , origTs'      :: TemplateState m
    , tsMVar'      :: MVar (TemplateState m)
    , sts'         :: StaticTagState
    }


renderTemplates :: MonadIO m
       => Templates m
       -> ByteString
       -> m (Maybe ByteString)
renderTemplates t n = do
    ts <- liftIO $ readMVar (tsMVar' t)
    renderTemplate ts n


reloadTemplates :: MonadIO m
                => Templates m
                -> m (Either String ())
reloadTemplates t = liftIO $ do
    clearStaticTagCache (sts' t)
    ets <- loadTemplates (templateDir' t) (origTs' t)
    leftPass ets $ \ts -> modifyMVar_ (tsMVar' t) (const $ return ts)


newTemplates :: MonadIO m
             => FilePath
             -> Splices m
             -> IO (Either String (Templates m))
newTemplates dir splices = liftIO $ do
    let spliceTs = foldr (uncurry bindSplice) emptyTemplateState splices
    (origTs,sts) <- bindStaticTag $ spliceTs
    ets <- loadTemplates dir origTs
    leftPass ets $ \ts -> do
        tsMVar <- newMVar $ ts
        return $ Templates dir origTs tsMVar sts

leftPass :: (Monad m) => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
     loadError = (++) "Error loading templates: "
