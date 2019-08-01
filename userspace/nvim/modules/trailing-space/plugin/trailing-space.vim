" No trailing spaces pls
function! StripTrailingWhitespace()
  " Save cursor position
  let l:save = winsaveview()
  " Remove trailing whitespace
  %s/\s\+$//e
  " Move cursor to original position
  call winrestview(l:save)
  echo "Stripped trailing whitespace"
endfunction

" But I sometimes want it dissabled
let g:noTrailingSpace = 1
function! AutoStripTrailingWhitespace()
  if g:noTrailingSpace == 1
    call StripTrailingWhitespace()
  endif
endfunction

autocmd BufWritePre * call AutoStripTrailingWhitespace()
