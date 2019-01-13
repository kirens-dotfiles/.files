{ xclip }:
''
function pbcopy
  ${xclip}/bin/xclip -o -selection clipboard
end pbcopy
''
