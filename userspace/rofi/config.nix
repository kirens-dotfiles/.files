{ writeTextFile }:
let
  style = writeTextFile {
    name = "rofi-style";
    text = import ./style.nix.rasi { };
  };
in ''
rofi.show-icons:            false
rofi.modi:                  drun,ssh,window
rofi.lines:                 7
rofi.line-padding:          10
rofi.matching:              normal
rofi.bw:                    0
rofi.padding:               0
rofi.separator-style:       none
rofi.hide-scrollbar:        true
rofi.line-margin:           0
rofi.font:                  sans-serif 10
rofi.theme:                 ${style}
''
