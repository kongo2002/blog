{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding                  ( id )
import           Control.Arrow                   ( (>>>), (***), (&&&), arr )
import           Control.Category                ( id )
import           Data.Monoid                     ( mempty, mconcat )
import           Data.List                       ( foldl' )
import           Data.Maybe                      ( catMaybes )
import qualified Data.Text as T

import           Text.Blaze.Html                 ( (!), toValue, toHtml )
import           Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll

main :: IO ()
main = hakyll $ do

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

    -- posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (renderDateField "shortdate" "%Y-%m-%d" "Date unknown")
            >>> arr (copyBodyToField "description")
            >>> renderTagsFieldCustom "posttags" tagIdentifier
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- about and projects page
    match (list [ "about.markdown", "projects.markdown" ]) $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- index page
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Recent posts")
        >>> requireAllA "posts/*" (id *** arr (take 10 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- atom feed
    match "feed.atom" $ route idRoute
    create "feed.atom" $ requireAll_ "posts/*"
        >>> renderAtom feedConfig

    -- tag cloud
    match "tags.html" $ route idRoute
    create "tags.html" $ constA mempty
        >>> arr (setField "title" "Tag cloud")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> applyTemplateCompiler "templates/tagcloud.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ route $ setExtension "html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- all posts page
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" (id *** arr (reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "templates/*" $ compile templateCompiler

  where
    tagIdentifier name =
        let sanitized = strRep [("#", "sharp"), (".", "dot")] name
        in  fromCapture "tags/*" sanitized

    replace s (a, b) =
        let [ss, aa, bb] = [T.pack x | x <- [s,a,b]]
        in  T.unpack $ T.replace aa bb ss

    strRep reps input = foldl' replace input reps

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 80 160

renderTagsFieldCombWith :: (Page a -> [String])
                    -> ([H.Html] -> [H.Html])
                    -> String
                    -> (String -> Identifier a)
                    -> Compiler (Page a) (Page a)
renderTagsFieldCombWith tags comb destination makeUrl =
    id &&& arr tags >>> setFieldA destination renderTags'
    where
      -- Compiler creating a comma-separated HTML string for a list of tags
      renderTags' :: Compiler [String] String
      renderTags' = arr (map $ id &&& makeUrl)
                  >>> mapCompiler (id *** getRouteFor)
                  >>> arr (map $ uncurry renderLink)
                  >>> arr (renderHtml . mconcat . comb . catMaybes)

      -- Render one tag link
      renderLink _ Nothing = Nothing
      renderLink tag (Just filePath) = Just $
          H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

renderTagsFieldCustom :: String
                    -> (String -> Identifier a)
                    -> Compiler (Page a) (Page a)
renderTagsFieldCustom = renderTagsFieldCombWith getTags (intersperseLast ", " " and ")

intersperseLast :: a -> a -> [a] -> [a]
intersperseLast _ _ []     = []
intersperseLast s l (x:xs) = x : prependLast s l xs

prependLast :: a -> a -> [a] -> [a]
prependLast _ _ []     = []
prependLast _ l [x]    = l : [x]
prependLast s l (y:ys) = s : y : prependLast s l ys

sass :: Compiler Resource String
sass = getResourceString
           >>> unixFilter "sass"
               ["-s", "--scss", "--style", "compressed"]

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
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

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
             { feedTitle = "uhlenheuer.net"
             , feedDescription = ".NET and programming blog from Gregor Uhlenheuer"
             , feedAuthorName = "Gregor Uhlenheuer"
             , feedAuthorEmail = "gregor@uhlenheuer.net"
             , feedRoot = "http://uhlenheuer.net"
             }
