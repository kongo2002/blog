---
tags: f#, .net, programming
author: Gregor Uhlenheuer
title: F# - Defining an extension method for generic arrays
summary: Ever wondered how to write an extension method for arrays in F#? This
         is how it can be done...
---
Lately I wanted to write a small extension method for the Array class in F#.
What would be a pretty simple task in C# appears to be a bit more tricky in F#.

~~~{.cs}
public static T GetOrDefault<T>(this T[] elements, int n)
{
    if (elements.Length > n)
        return elements[n];
    return default(T);
}
~~~

After trying a few things without any success I found a solution on
[StackOverflow][1] that I want to share with you.

Basically this is what it has to look like in F#:

~~~{.fsharp}
type 'a ``[]`` with
    member x.GetOrDefault(n) =
        if x.Length > n then x.[n]
        else Unchecked.defaultof<'a>
~~~

The trick is to use the backticks notation to define the array class. As stated
in the mentioned post you can extend via the ``IList<_>`` generic
interface as well:

~~~{.fsharp}
type System.Collections.Generic.IList<'T> with
    member x.GetOrDefault(n) =
        if x.Count > n then x.[n]
        else Unchecked.defaultof<'a>
~~~

[1]: http://stackoverflow.com/questions/11836167/how-to-define-a-type-extension-for-t-in-f/
