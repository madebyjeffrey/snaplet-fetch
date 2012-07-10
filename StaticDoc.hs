{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction         #-}
{-# LANGUAGE TemplateHaskell #-}

module StaticDoc where
-- (StaticDoc, staticInit, retrieve, retrieveDocument) where

import Data.Lens.Template
import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Snap
import Snap.Snaplet
import System.FilePath
import Debug.Trace
import Data.Maybe
import Data.Monoid
import System.Directory
import Snap.Snaplet.Heist
import Text.Templating.Heist
import qualified Text.XmlHtml as X


description :: T.Text
description = "Snaplet providing a source of documents"

data StaticDoc = StaticDoc
                  { documents :: FilePath                       -- path for markdown files
                  , images :: FilePath                          -- path for image files
                  , template :: String                          -- specific template to choose
                  }

staticInit :: (HasHeist b) => FilePath -> FilePath -> FilePath -> String -> SnapletInit b StaticDoc
staticInit documents images templates template = makeSnaplet "static-doc" description Nothing $ do
           (addTemplatesAt . TE.encodeUtf8 . T.pack $ (template)) templates
           addSplices [("insert", liftHeist markdownSplice)]
           return $ StaticDoc documents images template


markdownSplice :: Splice (Handler b v)
markdownSplice = do
               input <- getParamNode
               let text = T.unpack $ X.nodeText input
               return [X.TextNode $ T.pack text]

replaceText :: T.Text -> Splice (Handler b v)
replaceText text = do
            case X.parseHTML "" (TE.encodeUtf8 text) of
                 Left error -> return [X.TextNode $ T.pack error]
                 Right content -> return $ X.docContent content

-- change: we assume it is checked for existence
inputMarkdown :: FilePath -> IO T.Text
inputMarkdown path = do
                 fileContents <- readFile path
                 let doc = readMarkdown defaultParserState fileContents
                 let html = writeHtmlString defaultWriterOptions doc
                 pure $ T.pack html

-- assume that we have a route
suffix :: Handler b StaticDoc T.Text
suffix = do
       (Just route) <- getRoutePattern
       let route' = TE.decodeUtf8 route
       path <- withRequest (pure . TE.decodeUtf8 . rqURI)
       pure $ T.drop (T.length route') path

contentType :: Handler b StaticDoc T.Text
contentType = do
            path <- withRequest (pure . T.unpack . TE.decodeUtf8 . rqURI)
            case takeExtension path of
                 ".png" -> pure $ T.pack "image/png"
                 "" -> pure $ T.pack "text/html"
                 _ -> pure $ T.pack "text/plain"

exists :: FilePath -> Handler b StaticDoc ()
exists path = do
            s <- liftIO $ doesFileExist path
            case s of
                 True -> pure ()
                 False -> postError 404

postError :: Int -> Handler b StaticDoc ()
postError n = do
      modifyResponse $ setContentType "text/plain"
      modifyResponse $ setResponseStatus 404 "Not Found"
      writeBS "404 Can't find file"
      r <- getResponse
      finishWith r

retrieveImage :: Handler b StaticDoc ()
retrieveImage = do
              modifyResponse . setContentType . TE.encodeUtf8 =<< contentType

              s <- suffix              
              (StaticDoc _ imagePath _) <- get
              exists $ imagePath <> (T.unpack s)        -- ensure it exists
              sendFile $ imagePath <> (T.unpack s)
              
retrieveDocument :: Handler b StaticDoc ()
retrieveDocument = do
                 modifyResponse . setContentType . TE.encodeUtf8 =<< contentType

                 s <- suffix
                 (StaticDoc documentPath _ _) <- get
                 exists $ documentPath <> (T.unpack s) <.> "md"
                 writeText =<< (liftIO $ inputMarkdown (documentPath <> (T.unpack s) <.> "md"))

retrieve :: HasHeist b => Handler b StaticDoc ()
retrieve = do
         (Just route) <- getRoutePattern
         
         case route of
              "/images" -> retrieveImage 
              "" -> retrieveDocument
              _ -> postError 404


                 

                                                      
   