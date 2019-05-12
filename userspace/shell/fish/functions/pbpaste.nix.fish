{ xclip }:
''
function pbpaste
  ${xclip}/bin/xclip -o -selection clipboard
end
''
