{ }:
''
" Don't stay in insert mode if focus is lost
autocmd FocusLost * stopinsert

" Specify `updatetime` upon entering insert mode
autocmd InsertEnter * let updatetimePreInsert = &updatetime | set updatetime=30000
autocmd InsertLeave * let &updatetime = updatetimePreInsert

" When cursor has held for `updatetime` leave insert mode
autocmd CursorHoldI * stopinsert
''
