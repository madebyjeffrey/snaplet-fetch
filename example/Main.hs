{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- #define DEVELOPMENT

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import qualified Data.Text as T
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Core
import           System.IO
import           Site

-- #ifdef DEVELOPMENT
-- import           Snap.Loader.Dynamic
-- #else
import           Snap.Loader.Static
-- #endif

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]  'getActions  ["/Users/drakej/Code/SOCRweb/templates/clockworks"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineConfig defaultConfig


getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
