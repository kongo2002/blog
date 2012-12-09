---
tags: f#, .net, programming
author: Gregor Uhlenheuer
title: Out reference parameters in F#
summary: Ever wanted to create or override a function with an out parameter in
         F#? This is how it's done.
---

Lately I wanted to implement a class that derives from `DynamicObject` (in the
`System.Dynamic` namespace) and therefore I had to override the methods
`TrySetMember` and `TryGetMember`. Especially the latter one forced me to look
up on *reference parameters* in F#.

The method signature of `TryGetMember` looks like the following (in C# syntax):

~~~ {.cs}
public virtual bool TryGetMember(
    GetMemberBinder binder,
    out object result
)
~~~

In F# you have to declare the `result` parameter as a reference type with
`byref`:

~~~ {.fsharp}
open System.Runtime.InteropServices

override x.TryGetMember (binder : GetMemberBinder, [<Out>] result : byref<obj>) =
    raise <| NotImplementedException()
~~~

I am not quite sure if the `[<Out>]` parameter attribute is really necessary
since my project compiled just fine without it as well.
