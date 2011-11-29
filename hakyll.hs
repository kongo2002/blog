{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Prelude hiding (id)

import Hakyll

main :: IO ()
main = hakyll $ do

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> applyTemplateCompiler "templates/post.html"
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

    -- All posts page
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (id *** arr (reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

