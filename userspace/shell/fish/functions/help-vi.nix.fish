{ echo }:
''
function help-vi
  ${echo} 'Command mode'
  ${echo} '    h moves the cursor left.'
  ${echo} '    l moves the cursor right.'
  ${echo} '    i enters insert mode at the current cursor position.'
  ${echo} '    v enters visual mode at the current cursor position.'
  ${echo} '    a enters insert mode after the current cursor position.'
  ${echo} '    A enters insert mode at the end of the line.'
  ${echo} '    0 (zero) moves the cursor to beginning of line (remaining in'
  ${echo} '      command mode).'
  ${echo} '    dd deletes the current line and moves it to the killring.'
  ${echo} '    D deletes text after the current cursor position and moves it to'
  ${echo} '      the killring.'
  ${echo} '    p pastes text from the killring.'
  ${echo} '    u search history backwards.'
  ${echo} '    [ and ] search the command history for the previous/next token'
  ${echo} '      containing the token under the cursor before the search was'
  ${echo} '      started. See the history section for more information on'
  ${echo} '      history searching.'
  ${echo} '    Backspace moves the cursor left.'
  ${echo}
  ${echo} 'Visual mode'
  ${echo} '    ← and → extend the selection backward/forward by one character.'
  ${echo} '    b and w extend the selection backward/forward by one word.'
  ${echo} '    d and x move the selection to the killring and enter command mode.'
  ${echo} '    Escape and ControlC enter command mode.'
end
''
