{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction         #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
------------------------------------------------------------------------------
import           Application
import           StaticDoc

myHandler = do 
          writeText "Hello"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]

routes = [  ("/images", with staticDoc retrieve)
         ,  ("", with staticDoc retrieve)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" staticDoc $ staticInit "/Users/drakej/Code/SOCRweb/wiki" 
                                               "/Users/drakej/Code/SOCRweb/images"
                                               "/Users/drakej/Code/SOCRweb/templates"
                                               "clockworks"
    addRoutes routes
    return $ App s

