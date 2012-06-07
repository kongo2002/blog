---
title: F# - Singleton pattern
tags: programming, f#, .net
author: Gregor Uhlenheuer
summary: Implementing the singleton pattern in F#.
---
A few days ago I wondered how to implement the [Singleton pattern][1] in F#.
After a few failed attempts I came up with the following:

~~~ {.fsharp}
// Singleton type with a private parameterless constructor
type MySingleton private() =

    // other bindings

    // private static instance of the MySingleton type
    static let mutable instance = lazy(MySingleton())

    // public getter property
    static member Instance with get() = instance

    // other members
~~~

The point I had the most trouble with was the correct definition of a static
`let` binding. So in order to make a `let` binding static in a type definition
just put a `static` in front of it - that's all.

[1]: http://en.wikipedia.org/wiki/Singleton_pattern
