---
title: Haskell: Project Euler problem 2
tags: euler, haskell, programming
author: Gregor Uhlenheuer
summary: The next post in the upcoming Project Euler series
---

Today's post features the second problem of [Project Euler][euler] involving
the famous [Fibonacci numbers][fibonacci].

> Each new term in the Fibonacci sequence is generated by adding the previous
> two terms. By starting with 1 and 2, the first 10 terms will be:
>
> 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
>
> By considering the terms in the Fibonacci sequence whose values do not exceed
> four million, find the sum of the even-valued terms.

My first attempt used a straight-forward implementation of a single fibonacci
number and calculating each one less than four million separately:

~~~{.haskell}
fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

solution :: Int
solution =
    sum $ filter even $ takeWhile (<4000000) [fibonacci x | x <- [1..]]
~~~

Since this solution runs not very efficently I tried to come up with a slightly
improved version:

~~~{.haskell}
fibSeq :: Int -> [Int]
fibSeq 1 = [1]
fibSeq 2 = [1, 1]
fibSeq n = fibSeq' n [1,1]

fibSeq' :: Int -> [Int] -> [Int]
fibSeq' n list@(x:y:_) =
    if next > n then
        list
    else
        fibSeq' n (next:list)
    where
        next = x + y

solution2 :: Int
solution2 =
    sum $ filter even $ fibSeq 4000000
~~~

Now after reviewing the written code I notice that it is easily possible to
rewrite the `fibSeq'` function using [guards][guards]. The resulting function
looks more readable to me:

~~~{.haskell}
fibSeq' :: Int -> [Int] -> [Int]
fibSeq' n list@(x:y:_)
    | next > n = list
    | otherwise = fibSeq' n (next:list)
    where
        next = x + y
~~~

[euler]: http://projecteuler.net
[fibonacci]: http://en.wikipedia.org/wiki/Fibonacci_number
[guards]: http://en.wikibooks.org/wiki/Haskell/Control_structures#Guards
