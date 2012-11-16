---
title: Conway sequence in haskell
tags: haskell, programming, math
author: Gregor Uhlenheuer
summary: Generating the Conway look-and-say sequence in haskell
---

Inspired by a [F# snippet][snippet] on [fssnip.net][fssnip] that generated a
[Conway sequence][conway] look-and-say sequence I chose to implement the same
using [haskell][haskell].

The Conway sequence is built by reading the input string/sequence from left to
right and returning the number of repeated consecutive elements. Eg. `1211` is
converted to `111221` which in turn is processed into `312211`.

My pretty straight forward approach looks like the following:

~~~ {.haskell}
module Conway (getConway) where

import Data.List (unfoldr)

-- | 'getConway' generates an inifinite Conway look-and-say sequence
-- (sequence A006715 in OEIS). See
-- http://en.wikipedia.org/wiki/Look-and-say_sequence
--
-- A simple use of 'getConway':
--
-- > (take 3 $ getConway "abc") == ["abc","1a1b1c","111a111b111c"]
--
getConway :: String -> [String]
getConway input =
    unfoldr (\seed -> Just(seed, getNext seed)) input
    where
        grp x (Just l, c, lst) =
            if x == l then (Just l, (c+1), lst)
            else           (Just x, 1, (show c) ++ (l : lst))
        grp x (Nothing, _, _) = (Just x, 1, [])

        appendLast (a, b, c) =
            case a of
                Just chr  -> (show b) ++ chr : c
                otherwise -> c

        getNext = appendLast . foldr grp (Nothing, 0, [])
~~~

[snippet]: http://fssnip.net/fe
[fssnip]: http://fssnip.net/
[haskell]: http://haskell.org
[conway]: http://en.wikipedia.org/wiki/Look-and-say_sequence
