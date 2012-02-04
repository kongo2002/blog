---
title: F# - implementing an observable object
tags: f#, programming, .net, wpf
author: Gregor Uhlenheuer
summary: In case you want to have an observable object in F# for example to
         use it in a WPF application this is a convenient way to implement
         such thing.
---

Lately I wanted to use a WPF ListBox with selectable items (with a DataTemplate
and a simple CheckBox attached). In order to properly use such a thing you
should implement the `INotifyPropertyChanged` interface [^1]. I hadn't used
events in F# before so had to experiment a few things to get it right.

The solution was to implement a CheckItem class that wraps the selection
functionality and the implementation of the `INotifyPropertyChanged` interface.
This is what it looks like:

~~~{ .fsharp }
open System.ComponentModel
open Microsoft.FSharp.Quotations.Patterns

/// Observable object class implementing the INotifyPropertyChanged
/// interface
type ObservableItem() =
    let propertyChanged = Event<_,_>()
    let getPropertyName = function
        | PropertyGet(_, p, _) -> p.Name
        | _ -> invalidOp "Invalid expression argument: expecting property getter"

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged = propertyChanged.Publish

    member this.NotifyPropertyChanged name =
        propertyChanged.Trigger(this, PropertyChangedEventArgs(name))
    member this.NotifyPropertyChanged expr =
        expr |> getPropertyName |> this.NotifyPropertyChanged

/// Simple wrapper class for a selectable item
type CheckItem<'a>(item : 'a) =
    inherit ObservableItem()

    let mutable _isSelected = true
    let _item = item

    member this.Item
        with get () = _item

    member this.IsSelected
        with get () = _isSelected
        and set value =
            _isSelected <- value
            this.NotifyPropertyChanged <@ this.IsSelected @>
~~~

There are a few things worth noting here: The first interesting thing is the
attribute **CLIEventAttribute** [^2] that allows us the use a more consise way
of implementing events. Basically it adds the necessary CLI metadata to the
event and implements the *add_EventName* and *remove_EventName* methods.

The second interesting fact is the usage of **F# quotations** to attach the
right property name to the `PropertyChangedEventArgs` structure. Instead of
specifying a string constant it is possible to use a property expression.

[1]: http://msdn.microsoft.com/en-us/library/ee370437.aspx
[2]: http://msdn.microsoft.com/en-us/library/system.componentmodel.inotifypropertychanged.aspx

[^1]: [INotifyPropertyChanged on msdn][2]
[^2]: [CLIEventAttribute on msdn][1]
