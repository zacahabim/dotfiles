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

-- auto reload buffers after external modification

-- https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044
vim.api.nvim_create_autocmd({'FocusGained', 'BufEnter', 'CursorHold', 'CursorHoldI'}, {
  pattern = '*',
  command = "if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif",
})

vim.api.nvim_create_autocmd({'FileChangedShellPost'}, {
  pattern = '*',
  command = "echohl WarningMsg | echo 'File changed on disk. Buffer reloaded.' | echohl None",
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

-- netrw
vim.api.nvim_exec([[
let g:netrw_keepdir = 0
let g:netrw_preview = 1
let g:netrw_alto = 0
let g:netrw_liststyle = 0
let g:netrw_winsize = 30
let g:netrw_localcopydircmd = 'cp -r'
let g:netrw_localrmdir='rm -r'
hi! link netrwMarkFile Search
let g:netrw_banner = 0
]], false)

-- -- shell options
-- vim.api.nvim_exec([[
-- set shellcmdflag=-ic
-- ]], false)

-- pyindent
vim.api.nvim_exec([[
let g:pyindent_open_paren = 'shiftwidth()'
]], false)
