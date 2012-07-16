---
title: Scala: Project Euler problem 16
tags: euler, scala, programming
author: Gregor Uhlenheuer
summary: The solution of the 16. problem from Project Euler
---

This will be the solution of the sixteenth problem of Project Euler:

> 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
>
> What is the sum of the digits of the number 2^1000?

I am using my own version of `pow` here because it seems that the scala
standard method from `scala.math` does not support `BigInt` which is obviously
needed for this problem (or at least for my way of solving it \*g\*).

~~~{.scala}
class Euler016 extends Euler {

  def number = 16

  def solution = {

    def pow(base : Int, n : Int) = {
      def inner(base : BigInt, exp : Int, sum : BigInt) : BigInt = {
        exp match {
          case 1 => sum
          case _ => inner(base, exp-1, sum * base)
        }
      }
      inner(BigInt(base), n, BigInt(1))
    }

    val sum = pow(2, 1000).toString()
    sum.foldLeft(0)((s, c) => s + Character.getNumericValue(c))
  }
}
~~~
