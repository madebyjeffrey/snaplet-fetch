{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction         #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Snap.Snaplet.Fetch 
       ( fetchInit
       , fetchInitWithoutConfigFileTabernac
       , Fetch(..)
       , retrieve
       , retrieveDocument
       , retrieveBinary)
       where

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as H
import System.Exit
import Snap
import Data.Maybe
import System.FilePath
import Data.Monoid
import System.Directory
import Snap.Snaplet.Heist
import Text.Templating.Heist
import Snap.Util.FileServe
import qualified Text.XmlHtml as X
import qualified Text.Blaze.Renderer.XmlHtml as X2
import qualified Data.Configurator as C
import           Control.Monad.Trans.Writer

import Paths_snaplet_fetch

description :: T.Text
description = "Fetch provides a source of documents"

data Fetch = Fetch
                  { documentRoot :: FilePath                       -- document Root
                  , template :: String                          -- specific template to choose
                  }

data Config = Config String String String

dataDir :: IO [Char]
dataDir = liftM (++"/resources") getDataDir

logErr :: MonadIO m
       => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
logErr err m = do
    res <- liftIO m
    when (isNothing res) (tell [err])
    return res

(∆) :: Maybe a -> a1 -> Either a1 a
(∆) = flip $ (`maybe` Right) . Left


fetchInit :: (HasHeist b) => SnapletInit b Fetch
fetchInit = makeSnaplet "fetch" description (Just dataDir) $ do
          config <- getSnapletUserConfig
          (config', errs) <- runWriterT $ do
                  docroot <- logErr "Must specify a document root (docroot)" $ C.lookup config "docroot"
                  templateroot <- logErr "Must specify a template root (templateroot)" $ C.lookup config "templateroot"
                  theme <- logErr "Must specify a theme" $ C.lookup config "theme"
                  return $ Config <$> docroot <*> templateroot <*> theme
          case config' ∆ errs of
               Left errs' -> do
                    printInfo $ T.intercalate "\n" errs'
                    liftIO $ do putStrLn "Ack!"; exitWith (ExitFailure 1)
--                    fail "Errors cannot proceed"
--                    return $ Fetch "" ""   -- security??
               Right (Config docroot templateroot theme) -> do
                     addTemplatesAt "" templateroot
                     addSplices [("insert", liftHeist (markdownSplice docroot))]
                     addRoutes [ ("/themes", serveDirectory templateroot)
                               , ("", retrieve)
                               ]
                     return $ Fetch docroot theme

fetchInitWithoutConfigFileTabernac :: (HasHeist b) => FilePath -> FilePath -> String -> SnapletInit b Fetch
fetchInitWithoutConfigFileTabernac docRoot templateroot theme = makeSnaplet "fetch" description Nothing $ do
                                   addTemplatesAt "" templateroot
                                   addSplices [("insert", liftHeist (markdownSplice docRoot))]
                                   addRoutes [ ("/themes", serveDirectory templateroot)
                                             , ("", retrieve)
                                             ]
                                   return $ Fetch docRoot theme

--          let ci = fromMaybe (error $ intercalate "\n" errs) config
{-
          addTemplatesAt "" templates
          addSplices [("insert", liftHeist markdownSplice)]
          addRoutes [("", retrieveDocument)]

          return $ Fetch docs theme          
-}
{-

                          host <- logErr "Must specify postgres host" $ C.lookup config "host"


--           (addTemplatesAt . TE.encodeUtf8 . T.pack $ (template)) templates
           addTemplatesAt "" templates
           addSplices [("insert", liftHeist markdownSplice)]
           addRoutes [ ("", retrieveDocument ) ]

           return $ Fetch docs theme
-}

markdownSplice :: FilePath -> Splice (Handler b v)
markdownSplice documentPath = do
               input <- getParamNode
               let elements = X.elementAttrs input
               let attr = lookup "document" elements

               liftIO (print ( show elements))

--               (Fetch documentPath _) <-  get
--                 exists $ documentPath <> (T.unpack s) <.> "md"
--                 text <- liftIO $ inputMarkdown (documentPath <> (T.unpack s) <.> "md")

               case attr of
                    Just fileName -> liftIO $ inputMarkdown' (documentPath </> (T.unpack fileName))
                    Nothing -> return [X.TextNode $ T.pack "No document attribute found"]
--               let text = T.unpack $ X.nodeText input
  --             return [X.TextNode $ T.pack text]

replaceText :: T.Text -> Splice (Handler b v)
replaceText text = do
            case X.parseHTML "" (TE.encodeUtf8 text) of
                 Left err -> return [X.TextNode $ T.pack err]
                 Right content -> return $ X.docContent content

-- change: we assume it is checked for existence
inputMarkdown :: FilePath -> IO T.Text
inputMarkdown filePath = do
                 fileContents <- readFile filePath
                 let doc = readMarkdown defaultParserState fileContents
                 let html = writeHtmlString defaultWriterOptions doc
                 pure $ T.pack html

inputMarkdown' :: FilePath -> IO [X.Node]
inputMarkdown' filePath = do
                 fileContents <- readFile filePath
                 let doc = readMarkdown defaultParserState fileContents
                 let html = writeHtml defaultWriterOptions doc
                 pure $ X2.renderHtmlNodes html


-- assume that we have a route
suffix :: Handler b Fetch T.Text
suffix = do
       (Just theroute) <- getRoutePattern
       let route' = TE.decodeUtf8 theroute
       path' <- withRequest (pure . TE.decodeUtf8 . rqURI)
       pure $ T.drop (T.length route') path'

extension :: Handler b Fetch FilePath
extension = do
            filePath <- withRequest (pure . T.unpack . TE.decodeUtf8 . rqURI)
            pure $ takeExtension filePath

contentType :: Handler b Fetch T.Text
contentType = do
            filePath <- withRequest (pure . T.unpack . TE.decodeUtf8 . rqURI)
            case takeExtension filePath of
                 ".png" -> pure $ T.pack "image/png"
                 "" -> pure $ T.pack "text/html"
                 _ -> pure $ T.pack "text/plain"

exists :: FilePath -> Handler b Fetch ()
exists filePath = do
            s <- liftIO $ doesFileExist filePath
            case s of
                 True -> pure ()
                 False -> postError 404

postError :: Int -> Handler b Fetch ()
postError n = do
      modifyResponse $ setContentType "text/plain"
      modifyResponse $ setResponseStatus n "Not Found"
      writeBS "404 Can't find file"
      r <- getResponse
      finishWith r

retrieveBinary :: Handler b Fetch ()
retrieveBinary = do
--              modifyResponse . setContentType . TE.encodeUtf8 =<< contentType

              s <- suffix              
              (Fetch imagePath _) <- get
              exists $ imagePath <> (T.unpack s)        -- ensure it exists
              sendFile $ imagePath <> (T.unpack s)
              
retrieveDocument :: (HasHeist b) => Handler b Fetch ()
retrieveDocument = do
                -- modifyResponse . setContentType . TE.encodeUtf8 =<< contentType

                 s <- suffix
                 (Fetch documentPath theme) <- get
                 exists $ documentPath <> (T.unpack s) <.> "md"
                 text <- liftIO $ inputMarkdown (documentPath <> (T.unpack s) <.> "md")

--                 let s = withHeistTS $ hasTemplate "main"
--                 case s of 
--                      True -> writeText "Has main template"
--                      False -> writeText "No no main template"

                 templates <- withHeistTS templateNames; liftIO (print templates)
                 liftIO (print ( theme </> "main"))
--                 writeText text
                 renderWithSplices (TE.encodeUtf8 . T.pack $ theme </> "main") [("document", liftHeist $ replaceText text)]

--                 writeText =<< (liftIO $)

retrieve :: HasHeist b => Handler b Fetch ()
retrieve = do
         ext <- extension
         case ext of
              "" -> do
                        modifyResponse $ setContentType (H.lookupDefault "text/html" ".html" defaultMimeTypes)
                        retrieveDocument
              _ -> do 
                        modifyResponse $ setContentType (H.lookupDefault "text/html" ext defaultMimeTypes)
                        retrieveBinary

         



{-
do
         (Just route) <- getRoutePattern
         
         case route of
              "/images" -> retrieveImage 
              "" -> retrieveDocument
              _ -> postError 404

-}