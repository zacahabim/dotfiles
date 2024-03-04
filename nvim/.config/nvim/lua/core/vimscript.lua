-- Set status bar
vim.api.nvim_exec([[
set statusline=%m\                                " modified
set statusline+=%t                                " tail
set statusline+=\ [%{strlen(&fenc)?&fenc:'none'}, " file encoding
set statusline+=\ %{&ff}]                         " file format
set statusline+=\ %y                              " filetype
set statusline+=\ %r                              " read only flag
set statusline+=\ %h                              " help file flag
set statusline+=%=                                " left/right separator
set statusline+=%l:%c                             " line:column
set statusline+=\ (%P)                            " percentage
]], false)

-- Set autocmds
local autocmd = vim.api.nvim_create_autocmd

-- Remove whitespace on save
autocmd('BufWritePre', {
  pattern = '',
  command = ":%s/\\s\\+$//e"
})

-- Zoom window
vim.api.nvim_exec([[
function! s:zoom()
    if winnr('$') > 1
        tab split
    elseif len(filter(map(range(tabpagenr('$')), 'tabpagebuflist(v:val + 1)'),
                \ 'index(v:val, '.bufnr('').') >= 0')) > 1
        tabclose
    endif
endfunction
noremap <silent> <leader>zz :call <sid>zoom()<cr>
]], false)
