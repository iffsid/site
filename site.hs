{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad  (join, liftM, (>=>))
import           Data.Binary
import           Data.Monoid    ((<>))
import           Data.Typeable
-- import           Debug.Trace
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
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

  match  "css/*.css" $
    route idRoute >> compile compressCssCompiler

  -- clay for css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $
      liftM (fmap compressCss) $
      getResourceString >>= withItemBody (unixFilter "runghc" [])

  match "cv/cv.tex" $ do
    -- compile latex with rubber
    version "pdf" $ do
      route $ setExtension "pdf"
      compile $ getResourceLBS
        >>= withItemBody (unixFilterLBS "rubber-pipe" ["-m xelatex", "--into", "cv"])

  -- publications
  match "cv/association-for-computational-linguistics.csl" $ compile cslCompiler
  match "cv/references.bib" $ compile biblioCompiler

  match "cv/references.bib" $ version "html" $ do
    route $ delDir "cv/" `composeRoutes` setExtension "html"
    compile $ getResourceBody
      >>= return . fmap (\w -> "<pre>" ++ w ++ "</pre>")
      >>= defaultCompiler

  match "pages/publications.md" $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocBiblioCompiler
                "cv/association-for-computational-linguistics.csl"
                "cv/references.bib"
      >>= defaultCompiler

  -- main stuff
  match (fromList ["pages/index.html", "pages/bio.html"]) $ do
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
