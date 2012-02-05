---
title: WPF - using an ObjectDataProvider
tags: programming, .net, wpf
author: Gregor Uhlenheuer
summary: Since I always forget how to properly implement an ObjectDataProvider
         in WPF I will give a short explanation.
---

The `ObjectDataProvider` is a pretty neat construct in WPF to build custom data
providers for existing types. I find those especially useful when working with
enumerations. If you want to list all possible values of an enumeration in a
ComboBox, this are the steps you have to take.

First you have to create a static instance of the ObjectDataProvider in some
resource dictionary. In the resources of the current window this could look
like the following:

~~~{ .xml }
<Window.Resources>
    <ObjectDataProvider
        MethodName="GetValues"
        ObjectType="{x:Type sys:Enum}"
        x:Key="myEnumTypeProvider">
        <ObjectDataProvider.MethodParameters>
            <x:Type TypeName="local:MyEnumType"/>
        </ObjectDataProvider.MethodParameters>
    </ObjectDataProvider>
</Window.Resources>
~~~

The above lines are roughly equivalent to the invokation of `GetValues` in your
code-behind:

~~~{ .cs }
// the enumeration type
public enum MyEnumType
{
    First,
    Second,
    Third
}

// usage of Enum.GetValues to manually set the container's items
internal static void SetComboBoxValues(ComboBox container)
{
    if (container == null)
        throw new ArgumentNullException("container");

    var enumValues = Enum.GetValues(typeof(MyEnumType));
    container.ItemsSource = enumValues;
    container.SelectedIndex = 0;
}
~~~

The next step is to add a static binding for the `ItemsSource` of your
container to the new data provider:

~~~{ .xml }
<ComboBox
    Name="myTypeSelector"
    ItemsSource="{Binding Source={StaticResource myEnumTypeProvider}}"/>
~~~

Notice that you may have to add the necessary namespace declarations (for
*System in mscorlib* and your project's own namespace) in your XAML's header
like:

> xmlns:local="clr-namespace:MyProject"

## Discriminated unions in F\#

Using a *discriminated union* in F# sadly does not work the same way. In order
to obtain the types of a union type you have to use reflection and bind the
values manually to the container's `ItemsSource`.

~~~{ .fsharp }
open System.Windows.Controls
open Microsoft.FSharp.Reflection

type MyUnionType =
    | First
    | Second
    | Third

let getUnionNames =
    FSharpType.GetUnionCases typeof<MyUnionType>
    |> Array.map (fun t -> t.Name)

let setComboBoxValues (cb : ComboBox) =
    cb.ItemsSource <- getUnionNames
    cb.SelectedIndex <- 0
~~~

