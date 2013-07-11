{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (join, (>=>))
import           Data.Binary
import           Data.Monoid            ((<>))
import           Data.Typeable.Internal
import           Debug.Trace
import           Hakyll
-- import           System.FilePath        (takeBaseName, takeDirectory)
import           Text.Pandoc.Options    (writerHtml5)

pandocHtml5Compiler :: Compiler (Item String)
pandocHtml5Compiler =
  pandocCompilerWith defaultHakyllReaderOptions (defaultHakyllWriterOptions { writerHtml5 = True })

main :: IO ()
main = hakyll $ do
    -- copy as is
  match ("images/*" .||. "cv/cv.pdf" .||. "cv/cv.tex" .||. "publications/*.pdf") $
    route idRoute >> compile copyFileCompiler

  match "htaccess" $ route (gsubRoute "htaccess" (const ".htaccess")) >> compile copyFileCompiler

  match "pages/research/*/*.png" $ route (gsubRoute "pages/" (const "")) >> compile copyFileCompiler

    -- clay for css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    -- publications
  match "publications/*.markdown" $
    compile $ pandocHtml5Compiler >>= saveSnapshot "pubs" >>= defaultCompiler

  create ["publications.html"] $ do
    route idRoute
    compile $ makeItem "" >>= loadAndApplyTemplate elemsTemplate pubCtx >>= defaultCompiler

  create ["bibtex.html"] $ do
    route idRoute
    compile $ makeItem "" >>= loadAndApplyTemplate elemsTemplate bibCtx >>= defaultCompiler

    -- research page
  match "pages/research/short-*.markdown" $
    compile $ pandocHtml5Compiler >>= saveSnapshot "sdesc" >>= defaultCompiler

  match "pages/research/index.markdown" $ do
    route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler >>= applyAsTemplate descCtx >>= defaultCompiler

    -- main stuff
  match (fromList ["pages/index.html", "pages/reading.html", "pages/code.html"]) $ do
    route (gsubRoute "pages/" (const ""))
    compile $ getResourceBody >>= defaultCompiler

  match "templates/*" $ compile templateCompiler

mkT :: Identifier -> Identifier
mkT a = fromFilePath $ "templates/" ++ toFilePath a ++ ".html"

elemsTemplate :: Identifier
elemsTemplate = mkT "elements"

defaultCompiler :: Item String -> Compiler (Item String)
defaultCompiler = loadAndApplyTemplate "templates/default.html" defaultContext >=> relativizeUrls

eCtx :: (Item String -> Compiler String) -> Context String
eCtx fn = field "elements" fn <> defaultContext

bibCtx, pubCtx, descCtx :: Context String
bibCtx = eCtx (\_ -> eList (mkT "bibtex") recentFirst "publications/*.markdown" "pubs")
pubCtx = eCtx (\_ -> eList (mkT "publication") recentFirst "publications/*.markdown" "pubs")
descCtx = eCtx (\_ -> eList (mkT "short-description") return "pages/research/short-*.markdown" "sdesc")

eList :: (Typeable a, Binary a)
         => Identifier
         -> ([Item a] -> Compiler [Item String])
         -> Pattern
         -> Snapshot
         -> Compiler String
eList template sorter pattern name =
  join $ applyTemplateList
  <$> loadBody template
  <*> return defaultContext
  <*> (sorter =<< loadAllSnapshots pattern name)

-- todo
--  1. generate tags for all publications
--  2. for each research page, include publications whose tags match its tags
--  3. fill in the research pages' text
--  4. get Jeff to upload relevant videos to youtube under cccp account's username
--     get those videos linked to research page (and cccp's research pages)
-- http://chrisdone.com/posts/hakyll-and-git-for-you-blog
