highlight BadWhitespace ctermbg=darkgreen guibg=lightgreen

" Higlight trailing whitespace and tabs that are not at the start of a line
call matchadd('BadWhitespace', '\t\+$\|[^\t]\zs\t\+')

set showbreak=↪\ " yo
set list listchars=tab:\ \ ,nbsp:␣,trail:•,extends:⟩,precedes:⟨
