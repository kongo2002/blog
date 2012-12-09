---
tags: f#, visualstudio, .net, programming
author: Gregor Uhlenheuer
title: Using folders in F# projects in Visual Studio
summary: Mysteriously Visual Studio does not allow to add folders to F#
         projects. However it is perfectly possible to do so anyway.
---

For some strange reasons it is not possible to add folders to F# projects in
Visual Studio. Neither the 2010 edition nor the new 2012 version supports such
an important feature. However the good message is that it is indeed possible to
add folders on your own.

## Edit your project file

To add folders to your F# project you have to edit the project file
(`*.fsproj`) by hand using your favorite text editor. Search for the item group
with all the `Compile` statements:

~~~ {.xml}
  <!-- ... -->
  <ItemGroup>
    <Compile Include="Parser.fs" />
    <None Include="Script.fsx" />
  </ItemGroup>
  <!-- ... -->
~~~

Now you just have to add your new `Compile`, `Resource` or whatever statements
to that item group:

~~~ {.xml}
  <!-- ... -->
  <ItemGroup>
    <Resource Include="resources\images\icon.png" />
    <Resource Include="resources\images\error.png" />
    <Compile Include="Parser.fs" />
    <None Include="Script.fsx" />
  </ItemGroup>
  <!-- ... -->
~~~

After you matched your file system structure with your file paths you can
reload your project and enjoy your ordered project structure. Now you can even
add subfolders to already existing folders via the Visual Studio context menu.
