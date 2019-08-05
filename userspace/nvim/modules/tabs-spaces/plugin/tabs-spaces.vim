highlight InconsistentWhitespace ctermbg=darkred guibg=lightred

" Sets my prefrences for indentation and provides helpers for toggling
set tabstop=4

let g:tabsSpacesMatchId = -1
function UseTabs()
  set shiftwidth=4
  set softtabstop=0
  set noexpandtab
  if g:tabsSpacesMatchId > 0
    call matchdelete(g:tabsSpacesMatchId)
  endif
  let g:tabsSpacesMatchId = matchadd('InconsistentWhitespace', '^ \+')
endfunction

function UseSpaces()
  set shiftwidth=2
  set softtabstop=2
  set expandtab
  if g:tabsSpacesMatchId > 0
    call matchdelete(g:tabsSpacesMatchId)
  endif
  let g:tabsSpacesMatchId = matchadd('InconsistentWhitespace', '^\t\+')
endfunction

function ToggleTab()
  if &expandtab
    call UseTabs()
  else
    call UseSpaces()
  endif
endfunction

" Spaces > Tabs
call UseSpaces()
