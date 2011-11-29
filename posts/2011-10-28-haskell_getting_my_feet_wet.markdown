---
tags: programming, haskell
author: Gregor Uhlenheuer
title: Haskell - getting my feet wet
summary: Get a small impression about my ongoing learnings on functional
         programming. This time I am looking into Haskell!
---
This time it's Haskell - I bought myself a copy of the great book ["Real World
Haskell"][1] by Bryan O'Sullivan, John Goerzen and Don Stewart. Now I am just
working my way through the chapters and trying to practice along the exercises.

The first one is a simple function that calculates the lengths of a given list.
As far as I can see this function behaves pretty much the same like the
in-built `length` function.

~~~ {.haskell}
-- Write a function that computes the number of elements in a list. To test
-- it, ensure that it gives the same answers as the standard 'length'
-- function.

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs
~~~

The second one is supposed to calculate the mean of the elements in the given
list. My first approach used a self-written `sum` function using `foldr`.
Later I noticed that there is an in-built function called `sum` already that
I can use.

~~~ {.haskell}
-- Compute a function that computes the mean of a list, i.e., the sum of all
-- elements in the list divided by its length. (You may need to use the
-- 'fromIntegral' function to convert the length of the list from an integer
-- into a floating-point number.)

mean [] = 0
mean lst = sum lst / fromIntegral (len lst)
~~~

The next two ones are about [palindrome][2] numbers or collections. The first
one returns a palindrome list by simply appending the reverse of the input
list.

~~~ {.haskell}
-- Turn a list into a palindrome; i.e., it should read the same both
-- backward and forward. For example, given the list [1,2,3], your function
-- should return [1,2,3,3,2,1]

to_palindrome lst = lst ++ reverse lst
~~~

The second function determines if the given list is palindromic by sequencially
comparing the mirrored element pairs of the list.

~~~ {.haskell}
-- Write a function that determines whether its input list is a palindrome.

is_palindrome [] = False
is_palindrome lst =
    all (\x -> (lst !! x) == (lst !! (len-x-1))) [0..middle]
    where
        len = length lst
        middle = len `div` 2
~~~

Those functions do look really stupid simple but for an absolute beginner in
Haskell it was kind of a hassle especially to get all those *types* right.

But nevertheless it's quite interesting to slowly get a feel for those
functional programming constructs. Also it makes fun to see that you are indeed
able to solve these problems in the end after trying dozens of wrong
approaches.

[1]: http://book.realworldhaskell.org/
[2]: http://en.wikipedia.org/wiki/Palindrome
