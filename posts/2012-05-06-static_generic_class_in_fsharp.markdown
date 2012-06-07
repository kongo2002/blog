---
tags: f#, .net, generics
author: Gregor Uhlenheuer
title: F# - Static generic classes
summary: How to construct static generic classes in F#.
---
Recently I tried to build a static generic class in F# but as it seems there is
no direct way to do it. To see what I want to build, here is what could look
like in C#:

~~~ {.cs}
public static class ProcessorUnit<T>
    where T : IProcessor
{
    public static bool Process(T element)
    {
        return element.DoWork();
    }
}
~~~

The closest I could get in F# looks like the following. Since there are no
static classes in F# at all you have to use a type with a private constructor
and static member definitions:

~~~ {.fsharp}
type ProcessorUnit<'T when 'T :> IProcessor> private() =

    static let proc (element : 'T) =
        element.DoWork()

    static member Process(element) =
        proc element
~~~

