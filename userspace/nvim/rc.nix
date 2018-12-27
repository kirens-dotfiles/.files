{ }:
''
" Line numbering
set number
set relativenumber

" Margin
set colorcolumn=79

" Spaces > Tabs
set shiftwidth=2
set expandtab

" Default to X clipboard
set clipboard=unnamedplus

" No trailing spaces pls
autocmd BufWritePre * %s/\s\+$//e

" Break lines at cursor (oposite of J; J∘K == id)
nnoremap K i<CR><ESC>

" Don't stay in insert mode
autocmd FocusLost * stopinsert | wall!

" map mouse activity
set mouse=a


let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_null = "ø"
let g:javascript_conceal_this = "@"
let g:javascript_conceal_return = "⇚"
let g:javascript_conceal_undefined = "¿"
let g:javascript_conceal_NaN = "ℕ"
let g:javascript_conceal_prototype = "¶"
let g:javascript_conceal_static = "•"
let g:javascript_conceal_super = "Ω"
let g:javascript_conceal_arrow_function = "⇒"
let g:javascript_conceal_noarg_arrow_function = "○"
let g:javascript_conceal_underscore_arrow_function = "○"

set conceallevel=1


let g:solarized_termcolors = 256

''
