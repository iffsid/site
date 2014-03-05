{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (join, (>=>))
import           Data.Monoid            ((<>))
-- import           Debug.Trace
import           Hakyll
-- local imports
import           Extras.Options
import           Extras.Filters

main :: IO ()
main = hakyllWith hakyllConf $ do
  -- site control
  match "static/**" $ route (gsubRoute "static/" (const "")) >> compile copyFileCompiler

  -- copy as is
  match ("images/*" .||. "publications/*.pdf" .||. "css/*.css") $
    route idRoute >> compile copyFileCompiler

  -- clay for css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

  -- compile latex with rubber
  match ("cv/*.tex") $ do
    route $ setExtension "pdf"
    compile $ getResourceLBS >>= withItemBody (unixFilterLBS "rubber-pipe" ["-d"])

  -- publications
  match "publications/*.markdown" $
    compile $ pandocHtml5Compiler >>= saveSnapshot "pubs" >>= defaultCompiler

  create ["publications.html"] $ do
    route idRoute
    compile $ makeItem "" >>= loadAndApplyTemplate (mkT "elements") pubCtx >>= defaultCompiler

  create ["bibtex.html"] $ do
    route idRoute
    compile $ makeItem "" >>= loadAndApplyTemplate (mkT "elements") bibCtx >>= defaultCompiler

  -- research page
  match "pages/research/short-*.markdown" $
    compile $ pandocHtml5Compiler >>= saveSnapshot "sdesc" >>= defaultCompiler

  match ("pages/research/index.markdown" .||. "pages/research/*/index.markdown") $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
              >>= aplKeywords
              >>= applyAsTemplate descCtx
              >>= defaultCompiler

  match "pages/research/index.markdown" $ do
    route $ delDir "pages/" `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler >>= applyAsTemplate descCtx >>= defaultCompiler

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
  match (fromList ["pages/index.html", "pages/reading.html", "pages/code.html"]) $ do
    route $ delDir "pages/"
    compile $ getResourceBody >>= defaultCompiler

  --- need to figure out how to get 404 to work
  match "pages/404.html" $ do
    route $ delDir "pages/"
    compile $ getResourceBody >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "templates/*" $ compile templateCompiler

  where delDir = (flip gsubRoute) (const "")

mkT :: Identifier -> Identifier
mkT a = fromFilePath $ "templates/" ++ toFilePath a ++ ".html"

defaultCompiler :: Item String -> Compiler (Item String)
defaultCompiler = loadAndApplyTemplate "templates/default.html" defaultContext >=> relativizeUrls

eCtx :: (Item String -> Compiler String) -> Context String
eCtx fn = field "elements" fn <> defaultContext

bibCtx, pubCtx, descCtx :: Context String
bibCtx = eCtx (\_ -> eList (mkT "bibtex") recentFirst "publications/*.markdown" "pubs")
pubCtx = eCtx (\_ -> eList (mkT "publication") recentFirst "publications/*.markdown" "pubs")
descCtx = eCtx (\_ -> eList (mkT "short-description") recentFirst "pages/research/short-*.markdown" "sdesc")

eList template sorter pattern name =
  join $ applyTemplateList
  <$> loadBody template
  <*> return defaultContext
  <*> (sorter =<< loadAllSnapshots pattern name)

-- http://johnmacfarlane.net/pandoc/README.html#verbatim-code-blocks
-- http://vapaus.org/text/hakyll-configuration.html
-- http://johnmacfarlane.net/pandoc/scripting.html

-- todo
--  1. generate tags for all publications
--  2. for each research page, include publications whose tags match its tags
-- http://chrisdone.com/posts/hakyll-and-git-for-you-blog
