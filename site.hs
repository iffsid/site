{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Control.Monad (join)
import           Control.Applicative ((<$>),(<*>))
import           Hakyll
import           Debug.Trace

-- http://chrisdone.com/posts/hakyll-and-git-for-you-blog

-- :: Identifier
elemsTemplate = "templates/elements.html"
pubTemplate = "templates/publication.html"
bibTemplate = "templates/bibtex.html"
descTemplate = "templates/short-description.html"
defaultTemplate = "templates/default.html"

descCtx :: Context String
descCtx = (field "elements" (\_ -> descList)) <> defaultContext

descList :: Compiler String
descList = join $ applyTemplateList <$>
           (loadBody descTemplate) <*>
           (return defaultContext) <*>
           (loadAllSnapshots "pages/research/short-*.markdown" "short-desc")

bibCtx, pubCtx :: Context String
bibCtx = field "elements" (\_ -> pubList bibTemplate recentFirst) <> defaultContext
pubCtx = field "elements" (\_ -> pubList pubTemplate recentFirst) <> defaultContext

pubList template sortFilter =
    join $ applyTemplateList <$>
    (loadBody template) <*>
    (return defaultContext) <*>
    (sortFilter =<< loadAllSnapshots "publications/*/*.markdown" "pubs")

main :: IO ()
main = hakyll $ do
    -- copy as is
    match ("images/*" .||. "js/*" .||. "cv/cv.pdf" .||. "cv/cv.tex" .||.
           "publications/*/*.pdf" .||. "publications/*/image-thumbnail.png") $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/research/*/*.png" $ do
        route $ (gsubRoute "pages/" (const ""))
        compile copyFileCompiler

    -- clay for css
    match "css/*.hs" $ do
        route $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    -- publications
    match "publications/*/*.markdown" $ do
        compile $ do
            pandocCompiler
            >>= saveSnapshot "pubs"
            >>= loadAndApplyTemplate pubTemplate defaultContext
            >>= relativizeUrls

    create ["publications.html"] $ do
        route idRoute
        compile $ do
             makeItem ""
                  >>= loadAndApplyTemplate elemsTemplate pubCtx
                  >>= loadAndApplyTemplate defaultTemplate defaultContext
                  >>= relativizeUrls

    create ["bibtex.html"] $ do
        route idRoute
        compile $ do
             makeItem ""
                  >>= loadAndApplyTemplate elemsTemplate bibCtx
                  >>= loadAndApplyTemplate defaultTemplate defaultContext
                  >>= relativizeUrls

    -- research page
    match "pages/research/short-*.markdown" $ do
        compile $ do
            pandocCompiler
            >>= saveSnapshot "short-desc"
            >>= loadAndApplyTemplate descTemplate defaultContext
            >>= relativizeUrls

    match "pages/research/index.markdown" $ do
        route $ (gsubRoute "pages/" (const "")) `composeRoutes` (setExtension "html")
        compile $ do
             pandocCompiler
             >>= applyAsTemplate descCtx
             >>= loadAndApplyTemplate defaultTemplate descCtx
             >>= relativizeUrls

    -- main stuff
    match (fromList ["pages/index.html", "pages/reading.html"]) $ do
        route (gsubRoute "pages/" (const ""))
        compile $ do
             getResourceBody
             >>= loadAndApplyTemplate defaultTemplate defaultContext
             >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
