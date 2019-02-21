{ }:
''
${import ./dissableArrows.nix.vim { }}
${import ./leaveInsertMode.nix.vim { }}

" Line numbering
set number
set relativenumber

" Margin
set colorcolumn=80

" Autobreak
set textwidth=79
set wrap

" Spaces > Tabs
set shiftwidth=2
set expandtab

" Default to X clipboard
set clipboard=unnamedplus

" No trailing spaces pls
autocmd BufWritePre * %s/\s\+$//e

" Break lines at cursor (oposite of J; J∘K == id)
nnoremap K i<CR><ESC>l

" map mouse activity
set mouse=a

" Make word deletes stay in history
set virtualedit=onemore
inoremap <C-w> <C-o>db

" Folding
set foldmethod=syntax
set foldlevel=99

let g:solarized_termcolors = 256
''
