" Sets my prefrences for indentation and provides helpers for toggling
set tabstop=4

function UseTabs()
  set shiftwidth=4
  set softtabstop=0
  set noexpandtab
endfunction

function UseSpaces()
  set shiftwidth=2
  set softtabstop=2
  set expandtab
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
