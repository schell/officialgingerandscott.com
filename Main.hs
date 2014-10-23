{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*" $ do
        route $ matchRoute "pages/*.html"
              $ customRoute directoryIndex
        compile $ do
            let aboutCtx =
                    constField "title" "About" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            --posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    --listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/splash.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
directoryIndex :: Identifier -> FilePath
directoryIndex = (</> "index.html") . dropExtension . takeFileName . toFilePath
