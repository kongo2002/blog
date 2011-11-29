---
tags: programming, erlang
author: Gregor Uhlenheuer
title: Starting with Erlang
summary: As part of my current interest in functional programming I took a
         quick look at programming in Erlang.
---
Earlier last week we tried to use [ejabberd][1] at work for some testing. As
part of that we had to apply a patch to the ejabberd sources in order to add
some additional features we wanted to have.

Since there was no one really familiar with programming in [Erlang][2] I gave it
a shot. After half an hour of reading in google on Erlang syntax we managed to
patch the sources and add some additional logging information.

## Factorial function

Later at home I tried if I could come up with some basic functionality to kind
of get a feeling for the language. A common example when starting to learn a
new programming language is to implement the factorial function:

~~~ {.latex}
n! = \left\{
\begin{array}{l l}
    1 & \quad \text{if $n = 0$}\\
    n((n-1)!) & \quad \text{if $n > 0$}\\
\end{array}\right.
~~~

This is the first approach I came up with that uses recursion:

~~~ {.erlang}
fact(0) -> 1;
fact(X) when X > 0 -> X * fact(X-1).
~~~

The more interesting way of solving the above problem is to use
[tail-recursion][3]. That way the temporary processing values are stored in an
accumulator argument of the function that is being called. Using
*tail-recursion* it is not necessary that every step of the recursive function
has to be hold up in the stack.

~~~ {.erlang}
-module(factorial).
-export([fac/1]).

% API function that is being exporting
fac(N) -> fac(N,1).

% factorial function using tail-recursion
fac(0, Acc) -> Acc;
fac(N, Acc) when N > 0 -> fac(N-1, N*Acc).
~~~

### References

- [Erlang][2]
- [Learning You Some Erlang for Great Good][4]

[1]: http://www.ejabberd.im
[2]: http://www.erlang.org
[3]: http://en.wikipedia.org/wiki/Tail_call
[4]: http://learnyousomeerlang.com/content
