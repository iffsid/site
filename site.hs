{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad  (join, liftM, (>=>))
import           Data.Binary
import           Data.Monoid    ((<>))
import           Data.Typeable
-- import           Debug.Trace
import           Hakyll
-- local imports
import           Extras.Filters
import           Extras.Options

main :: IO ()
main = hakyllWith hakyllConf $ do
  -- site control
  match "static/**" $
    route (gsubRoute "static/" (const "")) >> compile copyFileCompiler

  -- copy as is
  match ("images/*" .||. "publications/*.pdf") $
    route idRoute >> compile copyFileCompiler

  -- copy as is
  match "js/jquery-proudify.min.js" $
    route idRoute >> compile copyFileCompiler

  match  "css/*.css" $
    route idRoute >> compile compressCssCompiler

  -- clay for css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $
      liftM (fmap compressCss) $
      getResourceString >>= withItemBody (unixFilter "runghc" [])

  match "cv/*.tex" $ do
    -- compile latex with rubber
    version "pdf" $ do
      route $ setExtension "pdf"
      compile $ getResourceLBS >>= withItemBody (unixFilterLBS "rubber-pipe" ["-m xelatex"])
    -- copy tex as is
    version "tex" $ route idRoute >> compile copyFileCompiler

  -- publications
  match "publications/*.markdown" $
    compile $ pandocHtml5Compiler >>= saveSnapshot "pubs" >>= defaultCompiler

  create ["publications.html"] $ do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate (mkT "elements") pubCtx
      >>= defaultCompiler

  create ["bibtex.html"] $ do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate (mkT "elements") bibCtx
      >>= defaultCompiler

  -- research page
  match "pages/research/short-*.markdown" $
    compile $ pandocHtml5Compiler
      >>= saveSnapshot "sdesc"
      >>= defaultCompiler

  match ("pages/research/index.markdown" .||. "pages/research/*/index.markdown") $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= aplKeywords
      >>= applyAsTemplate descCtx
      >>= defaultCompiler

  match "pages/research/index.markdown" $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= applyAsTemplate descCtx
      >>= defaultCompiler

  match "pages/research/*/*.markdown" $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= aplKeywords
      >>= applyAsTemplate descCtx
      >>= saveSnapshot "res"
      >>= defaultCompiler

  match ("pages/research/*/*.png" .||. "pages/research/*/*.jpg") $
    route (delDir "pages/") >> compile copyFileCompiler

  -- main stuff
  match (fromList ["pages/index.html", "pages/code.html"]) $ do
    -- "pages/reading.html",
    route $ delDir "pages/"
    compile $ getResourceBody >>= defaultCompiler

  match "pages/404.html" $ do
    route $ delDir "pages/"
    compile $ getResourceBody
              >>= (loadAndApplyTemplate "templates/default.html" defaultContext
                   >=> globalizeUrls "https://www.robots.ox.ac.uk/~nsid")
              -- http://www.iffsid.com

  match "templates/*" $ compile templateCompiler

  where delDir = flip gsubRoute (const "")

mkT :: Identifier -> Identifier
mkT a = fromFilePath $ "templates/" ++ toFilePath a ++ ".html"

defaultCompiler :: Item String -> Compiler (Item String)
defaultCompiler = loadAndApplyTemplate "templates/default.html" defaultContext >=> relativizeUrls

eCtx :: Compiler String -> Context String
eCtx expr = field "elements" (const expr) <> defaultContext

bibCtx, pubCtx, descCtx :: Context String
bibCtx  = eCtx $ eList "bibtex" recentFirst "publications/*.markdown" "pubs"
pubCtx  = eCtx $ eList "publication" recentFirst "publications/*.markdown" "pubs"
descCtx = eCtx $ eList "short-description" recentFirst "pages/research/short-*.markdown" "sdesc"

eList :: (Typeable a, Binary a) =>
         Identifier
      -> ([Item a] -> Compiler [Item String])
      -> Pattern
      -> Snapshot
      -> Compiler String
eList template sorter pat name =
  join $ applyTemplateList
  <$> loadBody (mkT template)
  <*> return defaultContext
  <*> (sorter =<< loadAllSnapshots pat name)

globalizeUrls :: String -> Item String -> Compiler (Item String)
globalizeUrls g item = do
    aRoute <- getRoute $ itemIdentifier item
    return $ case aRoute of
        Nothing -> item
        Just r  -> fmap (relativizeUrlsWith $ g ++ (tail $ toSiteRoot r)) item

-- http://johnmacfarlane.net/pandoc/README.html#verbatim-code-blocks
-- http://vapaus.org/text/hakyll-configuration.html
-- http://johnmacfarlane.net/pandoc/scripting.html

-- todo
--  1. generate tags for all publications
--  2. for each research page, include publications whose tags match its tags
-- http://chrisdone.com/posts/hakyll-and-git-for-you-blog
