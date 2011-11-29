---
tags: vim, programming
author: Gregor Uhlenheuer
title: Vim - visual search
summary: I recently came up with a small function to search for visually selected
         words in vim.
---
For all of you who want to search for a visually selected string in vim this
small function might come in handy:

~~~ {.Vim}
function! VSetSearch()
    let tmp = @@
    normal! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    call histadd('/', substitute(@/, '[?/]',
                \ '\="\\%d".char2nr(submatch(0))', 'g'))
    let @@ = tmp
endfunction

vnoremap * :<C-u>call VSetSearch()<CR>//<CR>
vnoremap # :<C-u>call VSetSearch()<CR>??<CR>
~~~

Now you can search as usual with `*` and `#` for the next and previous
search match of the currently highlighted text.
