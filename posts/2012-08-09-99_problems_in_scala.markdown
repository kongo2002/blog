---
title: 99 scala problems - part 1
tags: scala, programming
author: Gregor Uhlenheuer
summary: First part of a possibly longer series on solving 99 problems in scala
---

In order to get some more practice on programming in scala I started solving
some problems of the probably well known *99 problems* of other functional
programming languages. I first found that kind of series on the [haskell
wiki][1] that mentions the origin being the [Ninety-Nine Prolog Problems][2].

This post now contains my solutions to the first 10 problems mentioned on the
[haskell wiki][3] translated to scala. This first problems target the handling
of lists being one of the most important data structure in functional
programming.

### Problem 1: Find the last element of a list

~~~{.scala}
class Problem001 extends Problem {

  def number = 1

  def getLast[T](input : List[T]) : T = {
    input match {
      case Nil => throw new IllegalArgumentException("empty list")
      case head :: Nil => head
      case _ :: tail => getLast(tail)
    }
  }

  def test() = {
    getLast(List(1, 2, 3, 4)) == 4
  }
}
~~~

### Problem 2: Find the last but one element of a list

~~~{.scala}
class Problem002 extends Problem {

  def number = 2

  def lastButOne[T](list : List[T]) : T = {
    list match {
      case Nil => throw new IllegalArgumentException("empty list")
      case _ :: Nil => throw new IllegalArgumentException("list with one element only")
      case last :: _ :: Nil => last
      case _ :: tail => lastButOne(tail)
    }
  }

  def test() = {
    lastButOne(List(1,2,3,4)) == 3
  }
}
~~~

### Problem 3: Find the N-th element of a list

~~~{.scala}
class Problem003 extends Problem {

  def number = 3

  def getNth[T](list : List[T], n : Int) : T = {
    list match {
      case Nil => throw new IllegalArgumentException("index out of bounds")
      case head :: tail if n == 1 => head
      case _ :: tail => getNth(tail, n-1)
    }
  }

  def test() = {
    val lst = List(1,2,3,4,5)
    getNth(lst, 3) == 3 &&
    getNth(lst, 1) == 1 &&
    getNth(lst, 5) == 5
  }
}
~~~

### Problem 4: Count the elements in a list

The first element in the list is number 1.

~~~{.scala}
class Problem004 extends Problem {

  def number = 4

  def numElements[T](list : List[T]) = {
    def inner(lst : List[T], i : Int) : Int = {
      lst match {
        case Nil => i
        case _ :: tail => inner(tail, i+1)
      }
    }
    inner(list, 0)
  }

  def test() = {
    val lst = List.range(0, 10)

    numElements(lst) == 10 &&
    numElements(List()) == 0
  }
}
~~~

### Problem 5: Reverse a list

~~~{.scala}
class Problem005 extends Problem {

  def number = 5

  def rev[T](list : List[T]) = {
    def inner(lst : List[T], res : List[T]) : List[T] = {
      lst match {
        case Nil => res
        case head :: tail => inner(tail, head :: res)
      }
    }
    inner(list, List())
  }

  def test() = {
    rev(List(1,2,3,4,5)) == List(5,4,3,2,1)
  }
}
~~~

### Problem 6: Check whether a list is a palindrome

A [palindrome][4] can be read forward or backward (i.e. ``12321``)

~~~{.scala}
class Problem006 extends Problem {

  def number = 6

  def isPalindrome[T](list : List[T]) = {
    list == list.reverse
  }

  def test() = {
    isPalindrome(List(1,2,3,2,1)) &&
    isPalindrome("madamimadam".toList) &&
    !isPalindrome(List(1,3,1,2))
  }
}
~~~

### Problem 7: Flatten a list of lists

~~~{.scala}
class Problem007 extends Problem {

  def number = 7

  def flattenList[T](list : List[List[T]]) = {
    def inner(list : List[List[T]], res : List[T]) : List[T] = {
      list match {
        case Nil => res
        case head :: tail =>
          inner(tail, head.foldLeft(res)((l, h) => h :: l))
      }
    }
    inner(list, List()) reverse
  }

  def test() = {
    val lst = List(List(1,2), List(3), List(4,5,6))

    flattenList(lst) == List(1,2,3,4,5,6)
  }
}
~~~

### Problem 8: Eliminate consecutive duplicates of list elements

~~~{.scala}
class Problem008 extends Problem {

  def number = 8

  def compress[T](list : List[T]) = {
    def inner(lst : List[T], last : T, res : List[T]) : List[T] = {
      lst match {
        case Nil => res
        case head :: tail if head == last => inner(tail, last, res)
        case head :: tail => inner(tail, head, head :: res)
      }
    }
    list match {
      case Nil => Nil
      case head :: tail => inner(tail, head, List(head)) reverse
    }
  }

  def test() = {
    val lst = "aaabbbbbcccdefff".toList

    compress(lst) == "abcdef".toList
  }
}
~~~

### Problem 9: Pack consecutive duplicates of list elements into sublists

If a list contains repeated elements they should be placed in separate sublists.

~~~{.scala}
class Problem009 extends Problem {

  def number = 9

  def pack[T](list : List[T]) = {
    def inner(lst : List[T], last : List[T], res : List[List[T]]) : List[List[T]] = {
      lst match {
        case Nil => last :: res
        case hd :: tl if hd == last.head => inner(tl, hd :: last, res)
        case hd :: tl => inner(tl, List(hd), last :: res)
      }
    }
    list match {
      case Nil => Nil
      case head :: tail => inner(tail, List(head), List()) reverse
    }
  }

  def test() = {
    val lst = "aaaabbbcdeeee".toList

    pack(lst) == List("aaaa".toList, "bbb".toList, List('c'), List('d'), "eeee".toList)
  }
}
~~~

### Problem 10: Run-length encoding of a list

Use the result of problem 9 to implement the so-called *run-length encoding*
data compression method. Consecutive duplicates of elements are encoded as
tuples (``N E``) where ``N`` is the number of duplicates of the element ``E``.

~~~{.scala}
class Problem010 extends Problem {

  def number = 10

  def encode[T](list : List[T]) = {
    def inner(lst : List[T], last : (Int, T), res : List[(Int, T)]) : List[(Int, T)] = {
      val (count, elem) = last
      lst match {
        case Nil => last :: res
        case hd :: tl if hd == elem => inner(tl, last.copy(_1 = count+1), res)
        case hd :: tl => inner(tl, (1, hd), last :: res)
      }
    }
    list match {
      case Nil => Nil
      case hd :: tl => inner(tl, (1, hd), List()) reverse
    }
  }

  def test() = {
    val lst = "aaabbcddeeeee".toList

    encode(lst) == List((3, 'a'), (2, 'b'), (1, 'c'), (2, 'd'), (5, 'e'))
  }
}
~~~

[1]: http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems
[2]: https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/
[3]: http://www.haskell.org/haskellwiki/99_questions/1_to_10
[4]: http://en.wikipedia.org/wiki/Palindrome
