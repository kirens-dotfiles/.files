" Yank line
noremap Y y$

" Break lines at cursor (oposite of J; J∘K == id)
nnoremap K i<CR><ESC>l

" Insert empty lines in normal mode
nnoremap <CR> mxo<esc>`x

" Make word deletes stay in history
inoremap <C-w> <C-o>db
