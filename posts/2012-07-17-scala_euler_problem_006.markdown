---
title: Scala: Project Euler problem 6
tags: euler, scala, programming
author: Gregor Uhlenheuer
summary: Another Project Euler solution - number 6 this time
---

I guess I am rolling - here goes problem [number 6][1]:

~~~{.scala}
class Euler006 extends Euler {

  def number = 6

  def solution = {

    def sumOfSquares(n : Int) = {
      Stream.range(1, n+1, 1).map(x => x * x).sum
    }

    def squareOfSums(n : Int) = {
      val sums = Stream.range(1, n+1, 1).sum
      sums * sums
    }

    squareOfSums(100) - sumOfSquares(100)
  }
}
~~~

This one contains nothing too special at all. In fact I am not too sure if
there is any real benefit in using `Stream.range` instead of `Range`.

[1]: http://projecteuler.net/problem=6
