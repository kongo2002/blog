---
tags: f#, wpf, .net, programming
author: Gregor Uhlenheuer
title: Using WPF with F#
summary: I just recently tried to build a small WPF application in F# and
         although Visual Studio 2010 does not provide great support for WPF in
         concerns of F# the experience was not as bad as expected.
---
I just recently tried to build a small WPF application in F# and although
Visual Studio 2010 does not provide great support for WPF in concerns of F# the
experience was not as bad as expected.

I will give you a short walkthrough on how to create a new F# project in Visual
Studio and how to use WPF in there.

## Create a new project

The first thing you have to do is to create a new F# application in Visual Studio.

1. Use *File → New → Project...* to create a new project
2. Select *Visual F# → F# Application*
3. Add the following additional references:
    - `PresentationCore`
    - `PresentationFramework`
    - `System.Xaml`
    - `System.Xml`
    - `WindowsBase`

4. Open the *Properties* of your newly created project

5. Select **Windows Application** as *Output type* (in the *Application* tab)

## Application's entry point

Basically you are now ready to start hacking a fine WPF application in F#. In
order to define your application's entry point you may want to add the
following lines in your main source file:

~~~ {.fsharp}
namespace WpfSharpProject
open System
open System.Windows

module Program =

    [<STAThread>]
    [<EntryPoint>]
    let main args =
        SharpWindow().Run()
~~~

## Helper functions

In order to conveniently handle the WPF controls I found a few helper functions
to be very handy:

~~~ {.fsharp}
open System
open Microsoft.FSharp.Control

module Utils =

    /// Create a new window instance from the given XAML filename
    let window assembly name
        let uri = sprintf "/%s;component/%s" assembly name
        Application.LoadComponent(new Uri(uri, UriKind.Relative)) :?> Window

    /// Find the resource with the given name
    let (?) (window : Window) name =
        window.FindName name |> unbox

    /// Imitate the C# event handler syntax
    let inline (+=) (event : IEvent<_, _>) handler =
        (event :> IDelegateEvent<_>).AddHandler(RoutedEventHandler(handler))
~~~

Create a window
---------------

The last thing you have to do is to create a new XAML file, add its
**BuildAction** to `Resource` and add the matching code-behind F# source file.
You find a small example down below:

~~~ {.xml}
<!-- File: SharpWindow.xaml -->
<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Width="500" Height="400"
        Title="SharpWindow">
    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition Width="Auto"/>
            <ColumnDefinition/>
        </Grid.ColumnDefinitions>

        <!-- ... -->

    </Grid>
</Window>
~~~

The code-behind F# source file:

~~~ {.fsharp}
// File: SharpWindow.xaml.fs
namespace WpfSharpProject
open System.Windows
open System.Windows.Controls
open System.Windows.Data

type SharpWindow private (xaml : Window) as this =

    // associate a few controls
    let quit : Button = xaml?quitBtn
    let import : Button = xaml?importBtn
    let export : Button = xaml?exportBtn

    // connect a few event handlers
    do
        quit.Click += this.quitClick
        import.Click += this.importClick
        export.Click += this.exportClick

    new () =
        SharpWindow(window "WpfSharpProject" "SharpWindow.xaml")

    member this.Run() =
        (new Application()).Run xaml

    member this.quitClick sender args =
        xaml.Close()

    // ...
~~~

That's pretty much all you need to get started with your WPF hacking in F#. I
was very surprised how the creation of a small application like this was not
much more complicated than it would have been in C#.

So you don't have to be too frightened by Visual Studio 2010 not shipping a
builtin project template or sophisticated support for WPF and try it out
yourself!

### References

- [Kaxaml][1]: Open source XAML editor

[1]: http://kaxaml.com
