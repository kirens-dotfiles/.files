{ }:
''
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
nnoremap K i<CR><ESC>

" I need to forget arrows
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
noremap <C-Up> <C-y>
noremap <C-Down> <C-e>
noremap <C-Left> <NOP>
noremap <C-Right> <NOP>
vnoremap <Up> <NOP>
vnoremap <Down> <NOP>
vnoremap <Left> <NOP>
vnoremap <Right> <NOP>
vnoremap <C-Up> <C-y>
vnoremap <C-Down> <C-e>
vnoremap <C-Left> <NOP>
vnoremap <C-Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>
inoremap <C-Up> <NOP>
inoremap <C-Down> <NOP>
inoremap <C-Left> <NOP>
inoremap <C-Right> <NOP>

" Don't stay in insert mode
autocmd FocusLost * stopinsert | wall!

" map mouse activity
set mouse=a

let g:solarized_termcolors = 256

''
