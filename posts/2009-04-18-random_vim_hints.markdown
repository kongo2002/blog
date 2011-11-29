---
title: Random vim hints
tags: linux, vim, programming
author: Gregor Uhlenheuer
date: 2009-04-18
summary: I just picked some random vim stuff you might find useful
---

Today I want to share some of my recently discovered (or good old-timers) tips
for the vim editor.

If you are using highlighted search you can *un-highlight the last search* by
typing `:noh`

A few day ago I wondered if it is possible to use a column-selection in vim.
After a quick research I found the *block-wise visual mode* ``(CTRL-V)`` which
is exactly what I was searching for. A quick summary of the different visual
modes:

* `v` - character-wise visual mode
* `V` - line-wise visual mode
* `Ctrl-v` - block-wise visual mode (under win32 you can use the `Ctrl-q` key binding)

The next useful command is the *one-time insert command* `(CTRL-O)` which
allows you to ‘execute’ a single normal mode command while typing in insert
mode. A typical usage might be to mark a line you are currently typing in via
`CTRL-O ma`. That way you can continue typing in insert after marking the line.

If you are interested in *case-insensitive searching* the next one might be
interesting: Combining the settings `set ignorecase` and `set smartcase`
allows you to search case-insensitive by default but searches case-sensitive if
the search string contains uppercase characters.

The next one is a quick way to convert tabs into space characters:

~~~{.vim .numberLines}
:set expandtab
:retab!
~~~

The last one for today is the position of the current line in the window:

* `zz` - move current line to the center of the page
* `zt` - move current line to the top of the page
* `zb` - move current line to the bottom of the page
