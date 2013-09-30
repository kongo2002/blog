---
tags: vim, programming, linux
author: Gregor Uhlenheuer
title: Get going with vim
summary: Useful plugins and settings you may want to know when using vim for
         programming
---

To my mind [vim][1] is probably the best editor for programming and text
editing. However there are various helpful settings that are not set by most
distribution's default installations you may want to know to get the most out
of your vim editing experience. Moreover there are a bunch of [vim scripts][2]
that should not miss in your vim runtime files.


# vimrc

The following settings are not meant to be a fully functional or useful vim
configuration but more of a collection of snippets you could consider adding
to your own `.vimrc`. In my opinion you should not *blindly* copy other
people's vim configuration files without understanding every single setting
itself anyway.

However you can find a copy of my `.vimrc` on [github][3] for reference or
inspiration.

## Settings

### Basic editing

The most important non-default setting is called `hidden` which enables you to
switch between *buffers* without having to save in between. This is very
important in order to understand vim's concept of buffers and tabs which may
appear somewhat different compared to other popular text editors. See `:h
buffers` and `:h tabpage` for further information.

    " disable VI compatibility
    set nocompatible

    " enable buffer switching without having to save
    set hidden

    " allow backspace in insert mode
    set backspace=indent,eol,start

    " always activate automatic indentation
    set autoindent

### Display

    " display statusline even if there is only one window
    set laststatus=2

    " visually break lines
    set wrap

    " display line numbers
    set number

    " line numbers as narrow as possible
    set numberwidth=1

### Searching

    " turn on highlight search
    set hlsearch

    " ignore case in search when no uppercase search
    set incsearch
    set ignorecase
    set smartcase

Starting from a basic set of options you can incrementally improve or extend
your vim configuration by exploring vim's options. For one you can get help for
every setting by invoking `:h :<optionname>`. Moreover you can get a complete
list of available options via `:options`.


## Mappings

Find a few very basic key mappings you may find useful as well:

    " redraw screen and remove search highlights
    nnoremap <silent> <C-l> :noh<CR><C-l>

    " yank to end of line
    nnoremap Y y$

    " use Q for formatting
    noremap Q gq

    " easier navigation on wrapped lines
    nnoremap j gj
    nnoremap k gk


# Plugins

You may find a few of my favourite plugins below:

* [pathogen][5]: This plugin by [Tim Pope][4] is the undisputed must-have
  plugin for managing your vim runtime path and therefore your plugins. With
  *pathogen* you then can easily **manage your plugins** i.e. using git
  submodules.

* [syntastic][6]: Live **syntax checking** at its best - I am fully objective
  here although I am contributing to this amazing plugin :-)

* [surround][7]: Easily surround text with brackets, tags, keywords etc. (by
  [Tim Pope][4] as well)

* [fugitive][8]: Another one by [Tim Pope][4]: incredible **git integration**

* [Command-T][9]: Fast and intuitive **file searching** plugin written by
  *Wincent Colaiuta* (requires *ruby* enabled vim)

* [space][10]: "The Smart Space key for Vim" written by *Henrik Öhman*

* Just to name a few other great plugins you may want to check out: FSwitch,
  Tagbar, NerdCommenter, NerdTree ...

# Resources

* [vim.org][1]
* [vim scripts][2]
* [my .vimrc on github][3]


[1]: http://www.vim.org
[2]: http://www.vim.org/scripts
[3]: https://github.com/kongo2002/dotfiles/blob/master/.vimrc
[4]: https://github.com/tpope/
[5]: https://github.com/tpope/vim-pathogen
[6]: https://github.com/scrooloose/syntastic
[7]: https://github.com/tpope/vim-surround
[8]: https://github.com/tpope/vim-fugitive
[9]: https://wincent.com/products/command-t
[10]: https://github.com/spiiph/vim-space
