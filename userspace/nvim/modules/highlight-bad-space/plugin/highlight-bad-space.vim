highlight ExtraWhitespace ctermbg=darkgreen guibg=lightgreen

" Higlight trailing whitespace and tabs that are not at the start of a line
match ExtraWhitespace /\s\+$\|[^\t]\zs\t\+/
