---
tags: vim, plugin, programming
title: space.vim - smart space key for vim
author: Gregor Uhlenheuer
summary: I just happened to stumble over a really nice vim plugin called
         'space.vim' that turns the space key into a clever action key.
---
I want to present one of my all-time-favourite plugins for vim:
[space.vim][1]. This plugin written by Henrik Öhman maps the `<Space>`
key to act as a clever key to repeat all kinds of motions depending on the last
actions.

This way the plugin hooks i.e. into *search-commands* like `/`, `?`, `*`,
`#` as well as navigation in the *quickfix* window and jumping through
*diffs*.

I can strongly recommend this plugin as it heavily improves your navigation
speed and comfort just give it a try.

If you are interested I have got my own [fork][2] of the plugin on github. A
few changes by me are already merged into the author’s repository. A few days
ago I added support for *tag-movement* commands like `:tnext`, `:tprev` and
`<Ctrl-]>`

Links:
------

- [space.vim][1] on github
- my space.vim [fork][2] on github

[1]: http://github.com/spiiph/vim-space
[2]: http://github.com/kongo2002/vim-space
