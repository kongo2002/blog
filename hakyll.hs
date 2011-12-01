{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Prelude hiding (id)

import Hakyll

main :: IO ()
main = hakyll $ do

    -- Images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Static files
    match "static/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Css styles
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (renderDateField "shortdate" "%Y-%m-%d" "Date unknown")
            >>> renderTagsField "posttags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- About page
    match (list [ "about.markdown", "projects.markdown" ]) $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Index page
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Recent posts")
        >>> requireAllA "posts/*" (id *** arr (take 10 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tag cloud
    match "tags.html" $ route idRoute
    create "tags.html" $ constA mempty
        >>> arr (setField "title" "tag cloud")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> applyTemplateCompiler "templates/tagcloud.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ route $ setExtension "html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- All posts page
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (id *** arr (reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "templates/*" $ compile templateCompiler

  where
    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged '" ++ tag ++ "'"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
