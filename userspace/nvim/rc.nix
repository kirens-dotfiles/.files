{ }:
''
" Line numbering
set number
set relativenumber

" Spaces > Tabs
set shiftwidth=2
set expandtab

" Default to X clipboard
set clipboard=unnamedplus

" Margin
:set colorcolumn=79

" No trailing spaces pls
autocmd BufWritePre * %s/\s\+$//e

" map mouse activity
:set mouse=a
''
