---
title: Hakyll: static website generator in Haskell
tags: programming, haskell, web
author: Gregor Uhlenheuer
summary: As a part of my latest interest in functional programming and Haskell
         in particular, I switched my blog from rstblog to Hakyll.
---
As part of my latest interest in Haskell and functional programming in general
I went ahead and switched my static website generator to [Hakyll][2] that is
being written in Haskell. Before I was using [rstblog][1] which is written in
Python [^1].

## Hakyll

> Hakyll is a Haskell library for generating static sites, mostly aimed at
> small-to-medium sites and personal blogs. It is written in a very
> configurable way and uses an xmonad-like DSL[^2] for configuration.

Given the fact that Hakyll uses [pandoc][3] to parse and build the web pages
Hakyll can process [Markdown][4], [reStructuredText][5] or other popular text
formats. I tried to use reStructuredText with pandoc at first so I could reuse
my already written posts. Sadly that did not work out too good because I didn't
get syntax-highlighting to work properly.

Luckily converting to Markdown solved that problem and the conversion of my
posts from reStructuredText wasn't too complicated.

### Configuration

I am no export on Haskell yet at all so my current configuration is a pretty
basic one and consists of approximately 90% of the example configurations on the
Hakyll website. So it took some time to get comfortable with the [Hakyll][2]
DSL and the way it is supposed to be configured.

This is the way the blog posts are rendered:

~~~ {.haskell}
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
~~~

The following snippet illustrates the way the [tag cloud][7] is processed (though
I am not really happy with the output yet):

~~~ {.haskell}
match "tags.html" $ route idRoute
create "tags.html" $ constA mempty
    >>> arr (setField "title" "tag cloud")
    >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
    >>> applyTemplateCompiler "templates/tagcloud.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

-- ...
-- ...
-- ...

  where
    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120
~~~

## Summary

Overall I am pretty happy with the result so far. The blog looks nearly the
same as it looked before - anyways there is room for a lot of improvements.
These are a few points I would like to add in the near future:

- Pagination
- Automatic rendering of LaTeX formulas

So I guess the amount of spare time and my progress of learning Haskell will
influece when that will happen. If you have got critique or comments of any other kind feel free to send me an [email][8].


[1]: https://github.com/mitsuhiko/rstblog
[2]: http://jaspervdj.be/hakyll/
[3]: http://johnmacfarlane.net/pandoc/
[4]: http://daringfireball.net/projects/markdown/
[5]: http://docutils.sourceforge.net/rst.html
[6]: /posts/2011-09-15-new_blog.html
[7]: /tags.html
[8]: mailto:gregor@uhlenheuer.net

[^1]: [see my post on rstblog][6]
[^2]: Domain Specific Language
