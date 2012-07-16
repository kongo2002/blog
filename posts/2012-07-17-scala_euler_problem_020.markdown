---
title: Scala: Project Euler problem 20
tags: euler, scala, programming
author: Gregor Uhlenheuer
summary: Project Euler problem - the twentieth
---

This would be the factorial implementation in Scala combined with a small
function to sum the single digits of the resulting number.

~~~{.scala}
class Euler020 extends Euler {

  def number = 20

  def solution = {

    def factorial(n: Int) = {
      def inner(i: Int, current: BigInt): BigInt = {
        i match {
          case 1 => current
          case _ => inner(i - 1, current * i)
        }
      }
      inner(n, 1)
    }

    val fact = factorial(100).toString()
    fact.foldLeft(0)((s, c) => s + Character.getNumericValue(c))
  }
}
~~~
