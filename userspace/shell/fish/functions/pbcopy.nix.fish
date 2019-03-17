{ xclip }:
''
function pbcopy
  ${xclip}/bin/xclip -selection clipboard
end
''
