{ }:
''
${import ./dissableArrows.nix.vim { }}
${import ./leaveInsertMode.nix.vim { }}

syntax enable
set background=dark
colorscheme solarized

let g:signify_vcs_list = [ 'git', 'hg' ]

" Line numbering
set number
set relativenumber

" Margin
set colorcolumn=80
" More discrete color
highlight ColorColumn ctermbg=0

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

" Fold color
highlight Folded ctermbg=0

" Serach highlighting
highlight Search ctermbg=7

" More discrete line numbers
highlight LineNr ctermfg=10
highlight CursorLineNr ctermfg=12

let g:solarized_termcolors = 256

" Enable project specific .vimrc's
let g:localvimrc_whitelist='/e/.*'
set secure
''
