{ xorg }:
''
function brightness
  ${xorg.xrandr}/bin/xrandr --output eDP1 --brightness "''$argv[1]"
end
''
