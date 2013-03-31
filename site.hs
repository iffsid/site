{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (join)
import           Data.Binary
import           Data.Monoid            ((<>))
import           Data.Typeable.Internal
import           Debug.Trace
import           Hakyll
import           System.FilePath        (takeBaseName, takeDirectory)
import           Text.Pandoc.Options    (writerHtml5)

pandocHtml5Compiler :: Compiler (Item String)
pandocHtml5Compiler =
  pandocCompilerWith defaultHakyllReaderOptions (defaultHakyllWriterOptions { writerHtml5 = True })

main :: IO ()
main = hakyll $ do
    -- copy as is
  match ("images/*" .||. "cv/cv.pdf" .||. "cv/cv.tex" .||. "publications/*/*.pdf" .||.
         "publications/*/image-thumbnail.png") $ route idRoute >> compile copyFileCompiler

  match "pages/research/*/*.png" $ route (gsubRoute "pages/" (const "")) >> compile copyFileCompiler

    -- clay for css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    -- publications
  match "publications/*/*.markdown" $
    compile $ baseProcess $ pandocHtml5Compiler >>= saveSnapshot "pubs"

  create ["publications.html"] $ do
    route idRoute
    compile $ baseProcess $ makeItem "" >>= loadAndApplyTemplate elemsTemplate pubCtx

  create ["bibtex.html"] $ do
    route idRoute
    compile $ baseProcess $ makeItem "" >>= loadAndApplyTemplate elemsTemplate bibCtx

    -- research page
  match "pages/research/short-*.markdown" $
    compile $ baseProcess $ pandocHtml5Compiler >>= saveSnapshot "short-desc"

  match "pages/research/index.markdown" $ do
    route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
    compile $ baseProcess $ pandocHtml5Compiler >>= applyAsTemplate descCtx

    -- main stuff
  match (fromList ["pages/index.html", "pages/reading.html"]) $ do
    route (gsubRoute "pages/" (const ""))
    compile $ baseProcess getResourceBody

  match "templates/*" $ compile templateCompiler

baseProcess :: Compiler (Item String) -> Compiler (Item String)
baseProcess m = m >>= loadAndApplyTemplate defaultTemplate defaultContext >>= relativizeUrls

elemsTemplate, pubTemplate, bibTemplate, descTemplate, defaultTemplate :: Identifier
elemsTemplate = "templates/elements.html"
pubTemplate = "templates/publication.html"
bibTemplate = "templates/bibtex.html"
descTemplate = "templates/short-description.html"
defaultTemplate = "templates/default.html"

bibCtx, pubCtx, descCtx :: Context String
bibCtx = field "elements" (\_ -> elemList bibTemplate recentFirst "publications/*/*.markdown" "pubs")
         <> defaultContext
pubCtx = field "elements" (\_ -> elemList pubTemplate recentFirst "publications/*/*.markdown" "pubs")
         <> defaultContext
descCtx = field "elements" (\_ -> elemList descTemplate return "pages/research/short-*.markdown" "short-desc")
          <> defaultContext

elemList :: (Typeable a, Binary a) =>
            Identifier
            -> ([Item a] -> Compiler [Item String])
            -> Pattern
            -> Snapshot
            -> Compiler String
elemList template sorter pattern name =
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
