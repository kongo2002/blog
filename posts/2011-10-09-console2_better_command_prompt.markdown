---
tags: windows, tools, console
author: Gregor Uhlenheuer
date: 2011-10-09
title: Console2 - the better command prompt
summary: How to setup console2 to be an awesome cmd.exe replacement.
---
For all of you that don't know *Console2*: it's a Windows console replacement
that among others includes support for multiple tabs, text-editor like
selection, transparency and configurable fonts.

Now that I just happened to visit [Scott Hanselman's blog][1] in order to read
about a good initial startup configuration I decided to write a short post
about [Console2][2] too.

## Installation

1. Just go to the project's site on sourceforge and download the latest version
   available: <http://sourceforge.net/projects/console/files/>
2. Extract the contents of the downloaded archive into a directory of your
   liking. I would recommend a folder that is in your *PATH* (in my case
   `d:\bin\`).
3. You are now ready to go by launching the `Console2.exe` executable

## Configuration

The following configuration example is heavily inspired by Scott Hanselman's
[great blog post about Console2][3] and slightly modified to my own preference.
The following points are a few recommendations on how to configure *Console2*:

- Start Console2
- Open the settings on `Edit → Settings`
- Configure your **Startup directory** under `Console`
- **Hide** the menu, status bar and tool bar under `Appearance → More`
- Set your font to **Consolas** under `Appearance`
- Set **Copy on Select** under `Behavior`
- Switch to the **Hotkeys** menu:
    * Set **New Tab1** to `Ctrl-T`
    * Set **Copy Selection** to `Ctrl-C`
    * Set **Paste** to `Ctrl-V`
- Configure some default tabs:
    * **VS 2010 prompt**: `%comspec% /k "D:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vsvarsall.bat"`
    * **Powershell**: `%SystemRoot%\syswow64\WindowsPowerShell\v1.0\powershell.exe`
    * **git**: `D:\msysgit\bin\sh.exe --login -i`

If you intend to use Console2 with git, I would recommend to *not* modify your
default font color unlike Scott Hanselman suggests. With a modified font color
you will loose the shell coloring git provides i.e. on using commands like
`git diff`.

[1]: http://hanselman.com
[2]: http://sourceforge.net/projects/console/
[3]: http://www.hanselman.com/blog/Console2ABetterWindowsCommandPrompt.aspx
