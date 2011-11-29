---
tags: wpf, xaml, .net
title: WPF: Use a resource image from XAML
author: Gregor Uhlenheuer
summary: I always forget how to use a resource image file in a XAML file - here
         is how it's done.
---
Since I always forget how to reference a resource image file from inside a XAML
file I will quickly describe how it can be done.

Say you have added an existing image file to your a project with the namespace
`Project.Tools`. Now all you have to do is to set the *Build Action* of the
file to `Resource` and reference the image like the following:

~~~ {.xml}
<MenuItem.Icon>
    <Image Source="/Project.Tools;component/Resources/Images/image.png" Height="16" Width="16"/>
</MenuItem.Icon>
~~~

In the example above the image is located inside the project in a directory
structure of `/Resources/Images/`.

### References

- [Pack URIs in WPF][1]

[1]: http://msdn.microsoft.com/en-us/library/aa970069.aspx
