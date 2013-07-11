    "publications/*/image-thumbnail.png"
    tags <- buildTags "publications/*/*.markdown" (fromCapture "aaa")

    create ["foo.html"] $ do
        route idRoute
        compile $ do
            -- str <- renderTagList (traceShow tags tags)
            str <- renderTagList tags
            makeItem str
                  >>= loadAndApplyTemplate defaultTemplate defaultContext
                  >>= relativizeUrls

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) <>
                    constField "title" "Archives"             <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath

indexCtx = field "posts" $ \_ -> postList $ fmap (take 3) . recentFirst

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postList            :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

pubList            :: ([Item String] -> Compiler [Item String]) -> Compiler String
pubList sortFilter = do
    pubs    <- sortFilter =<< loadAll "publications/*"
    itemTpl <- loadBody "templates/publication.html"
    list    <- applyTemplateList itemTpl pubCtx pubs
    return list

    create ["research/index.html"] $ do
        route idRoute
        compile $ do
             makeItem ""
                  >>= loadAndApplyTemplate elemsTemplate descCtx
                  >>= loadAndApplyTemplate defaultTemplate defaultContext
                  >>= relativizeUrls
