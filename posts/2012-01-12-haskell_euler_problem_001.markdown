---
title: Haskell: Project Euler problem 1
tags: euler, haskell, programming
author: Gregor Uhlenheuer
summary: Solution of the first problem of Project Euler
---

This is my start of hopefully a series of posts on solving the problems of
[Project Euler](http://projecteuler.net). So let's start with the first problem:

The problem's description is as following:

> If we list all the natural numbers below 10 that are multiples of 3 or 5,
> we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

I am pretty sure there are more elegant solutions to this fairly simple
problem, but this is my first shot:

~~~{.haskell}
solution :: Int
solution =
    sum multiples
    where
        multiples = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
~~~
