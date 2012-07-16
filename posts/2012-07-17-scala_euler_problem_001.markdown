---
title: Scala: Project Euler problem 1
tags: euler, scala, programming
author: Gregor Uhlenheuer
summary: Solution of the first problem of Project Euler written in Scala
---

Once again I misuse some of the fairly simple problem from Project Euler to
fiddle around with a new programming language - this time it's Scala.

So in case you haven't read one of my earlier post on Project Euler this is the
problems description:

> If we list all the natural numbers below 10 that are multiples of 3 or 5,
> we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

My scala solution looks like this:

~~~{.scala}
class Euler001 extends Euler {

  def number = 1

  def solution = {
    val sequence = for (i <- 1 until 1000 if i % 3 == 0 || i % 5 == 0) yield i
    sequence sum
  }
}
~~~
