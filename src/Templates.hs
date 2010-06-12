module Templates
    ( Templates
    , newTemplates

    , getTs
    , refresh

    , addSplices
    ) where
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


data Templates m = Templates
    { templateDir :: FilePath
    , origTs      :: TemplateState m
    , tsMVar      :: MVar (TemplateState m)
    , sts         :: StaticTagState
    }


refresh :: MonadIO m => Templates m -> m (Either String ())
refresh t = liftIO $ do
    clearStaticTagCache (sts t)
    ets <- loadTemplates (templateDir t) (origTs t)
    leftPass ets $ \ts -> modifyMVar_ (tsMVar t) (const $ return ts)


newTemplates :: MonadIO m
             => FilePath
             -> [(ByteString, Splice m)]
             -> IO (Either String (Templates m))
newTemplates dir splices = do
    (origTs',sts') <- bindStaticTag $ addSplices splices emptyTemplateState
    ets <- loadTemplates dir origTs'
    leftPass ets $ \ts -> do
        tsMVar' <- newMVar $ ts
        return $ Templates dir origTs' tsMVar' sts'


getTs :: MonadIO m => Templates m -> m (TemplateState m)
getTs t = liftIO $ readMVar $ tsMVar t


addSplices :: Monad m
           => [(ByteString, Splice m)]
           -> TemplateState m
           -> TemplateState m
addSplices = flip $ foldr (uncurry bindSplice)


leftPass :: Monad m => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
     loadError = (++) "Error loading templates: "
