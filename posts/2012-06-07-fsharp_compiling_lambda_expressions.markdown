---
tags: f#, .net, programming, reflection
author: Gregor Uhlenheuer
title: F# - Compiling lambda expressions
summary: Ever tried to compile a lambda expression in F# into an internal
         delegate type? Well, if you actually did you will know it does not
         work as you probably would expect.
---
If you try to compile a lambda expression in F# into an internal delegate type
you should be warned because this won't work as you would expect it from C#
behavior for example.

In order to better illustrate what I mean here is a small example (which
actually does not work):

~~~ {.fsharp}
module internal ReflectionHelpers =

    open System
    open System.Linq.Expressions
    open System.Reflection

    // this is the delegate type we want to use
    type GetterFunc<'T> = delegate of 'T -> obj

    let getGetter<'a> (p : PropertyInfo) =
        let inst = Expression.Parameter(p.DeclaringType, "i")
        let prop = Expression.Property(inst, p)
        let conv = Expression.Convert(prop, typeof<obj>)

        // this will throw an ArgumentNullException
        Expression.Lambda<GetterFunc<'a>>(conv, inst).Compile()
~~~

The above code snippet compiles just fine but on execution you will get an
`ArgumentNullException`. The problem is somewhat hidden because the method
`Expression.Lambda` tries to find a *public* `Invoke` method on the given
delegate type. This works on C# as expected but in F# the `Invoke` method is
defined with the same visibility as the declaring type (which is `internal` in
this example).

As of now you only have to workarounds to choose from:

1. Either use a public delegate type
2. or use an internal delegate in C# code an reference it via
   `InternalsVisibleTo`
