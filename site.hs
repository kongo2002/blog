{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding                  ( id )

import           Control.Applicative             ( (<$>) )
import           Control.Monad                   ( forM )

import           Data.Monoid                     ( mconcat, mappend )
import           Data.List                       ( foldl', intersperse, nub )
import           Data.Maybe                      ( catMaybes )
import qualified Data.Text as T

import           System.Environment              ( getArgs )

import           Text.Blaze.Html                 ( (!), toValue, toHtml )
import           Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll


main :: IO ()
main = do

    -- fetch command line arguments
    (action:args) <- getArgs

    -- 'preview mode' determination
    let previewMode = action == "preview"
        -- configuration
        config      = if previewMode
                      then previewConfiguration
                      else defaultConfiguration

        -- pattern depending on 'preview mode'
        getPattern pat = if previewMode
                         then posts .||. drafts
                         else posts
                       where
                         posts  = fromGlob $ pat ++ "/*"
                         drafts = fromGlob $ "drafts/" ++ pat ++ "/*"
        postsPattern = getPattern "posts"

    hakyllWith config $ do

        -- images
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        -- static files
        match "static/*" $ do
            route idRoute
            compile copyFileCompiler

        -- css styles
        match "css/*" $ do
            route $ setExtension "css"
            compile sass

        -- build tags
        tags <- buildTags postsPattern tagIdentifier

        -- posts
        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" postContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- about and projects page
        match ("about.markdown" .||. "projects.markdown" ) $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        -- tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged '" ++ tag ++ "'"

            route idRoute
            compile $ do
                let context = constField "title" title `mappend`
                              listField "posts" postContext (recentFirst =<< loadAll pattern) `mappend`
                              defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" context
                    >>= loadAndApplyTemplate "templates/default.html" context

        -- index page
        create ["index.html"] $ do
            route idRoute
            compile $ do
                let context = constField "title" "Recent posts" `mappend`
                              listField "posts" postContext (fmap (take 10) . recentFirst =<< loadAll postsPattern) `mappend`
                              defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" context
                    >>= loadAndApplyTemplate "templates/default.html" context

        -- tag cloud
        create ["tags.html"] $ do
            route idRoute
            compile $ do
                cloud <- renderTagCloud' tags
                let context = constField "title" "Tag cloud" `mappend`
                              constField "tagcloud" cloud `mappend`
                              defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/tagcloud.html" context
                    >>= loadAndApplyTemplate "templates/default.html" context

        -- atom feed
        create ["feed.atom"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots postsPattern "content"
                    >>= recentFirst
                    >>= renderAtom feedConfig feedContext

        -- all posts page
        create ["posts.html"] $ do
            route idRoute
            compile $ do
                let context = constField "title" "All posts" `mappend`
                              listField "posts" postContext (recentFirst =<< loadAll postsPattern) `mappend`
                              defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" context
                    >>= loadAndApplyTemplate "templates/default.html" context

        match "templates/*" $ compile $ templateCompiler

      where
        renderTagCloud' = renderTagCloud 80 160


tagsFieldCombWith :: (Identifier -> Compiler [String])
                  -> ([H.Html] -> [H.Html])
                  -> String
                  -> Context a
tagsFieldCombWith getTags' comb key = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        route' <- getRoute $ tagIdentifier tag
        return $ renderLink tag route'

    return $ renderHtml $ mconcat $ comb $ catMaybes $ links
  where
    -- Render one tag link
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
        H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag


tagsFieldCustom :: String -> Context a
tagsFieldCustom =
    tagsFieldCombWith getTags (intersperseLast ", " " and ")


intersperseLast :: a -> a -> [a] -> [a]
intersperseLast _ _ []     = []
intersperseLast s l (x:xs) = x : prependLast s l xs


prependLast :: a -> a -> [a] -> [a]
prependLast _ _ []     = []
prependLast _ l [x]    = l : [x]
prependLast s l (y:ys) = s : y : prependLast s l ys


tagIdentifier :: String -> Identifier
tagIdentifier name =
    let sanitized = strRep [("#", "sharp"), (".", "dot")] name
    in  fromCapture "tags/*.html" sanitized


keywordsField = field "keywords" $ \item -> do
    tags <- getTags $ itemIdentifier item
    return $ build tags
  where
    build = concat . intersperse "," . nub . (++ defKeywords)
    defKeywords = [ "dotnet"
                  , "programming"
                  , ".NET"
                  , "linux"
                  , "C#"
                  , "F#"
                  , "functional"
                  ]


replace :: String -> (String, String) -> String
replace s (a, b) =
    let [ss, aa, bb] = [T.pack x | x <- [s,a,b]]
    in  T.unpack $ T.replace aa bb ss


strRep :: [(String, String)] -> String -> String
strRep reps input = foldl' replace input reps


sass :: Compiler (Item String)
sass =
    getResourceString >>=
        withItemBody (unixFilter "sass" ["-s", "--scss", "--style", "compressed"])


postContext :: Context String
postContext = mconcat
    [ dateField "date" "%B %e, %Y"
    , dateField "shortdate" "%Y-%m-%d"
    , modificationTimeField "mtime" "%U"
    , tagsFieldCustom "posttags"
    , keywordsField
    , defaultContext
    ]


feedContext :: Context String
feedContext = mconcat
    [ bodyField "description"
    , defaultContext
    ]


feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
             { feedTitle = "uhlenheuer.net"
             , feedDescription = ".NET and programming blog from Gregor Uhlenheuer"
             , feedAuthorName = "Gregor Uhlenheuer"
             , feedAuthorEmail = "gregor@uhlenheuer.net"
             , feedRoot = "http://uhlenheuer.net"
             }


previewConfiguration :: Configuration
previewConfiguration = defaultConfiguration
    { destinationDirectory = "_preview/out"
    , storeDirectory = "_preview/cache"
    , tmpDirectory = "_preview/cache/tmp"
    }
