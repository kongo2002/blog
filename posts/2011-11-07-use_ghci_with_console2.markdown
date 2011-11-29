---
tags: programming, haskell, console, tools, windows
title: Haskell: using ghci with Console2
author: Gregor Uhlenheuer
summary: For all you haskell programmers out there having or choosing to use
         windows, using Console2 is a great way to improve the ghci
         experience.
---
One great thing about haskell and and its *Glasgow compiler* is `ghci`, the
interactive compiler. Using *ghci* you can quickly test and reevalute code you
have just written.

Today I just installed the Haskell platform for Windows. After first trying the
*ghci* I just remembered how crappy `cmd.exe` is. But rescue is near - go
ahead and setup Console2 to use ghci as one tab setting.

## How to setup Console2

1. First install Haskell platform:
   <http://hackage.haskell.org/platform/windows.html>
2. Open Console2 and go to *Edit → Settings ...*
3. Create a new tab setting
    - Set **shell** to `cmd.exe /K "ghci.exe"`
