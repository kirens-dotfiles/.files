module Xmobar.MyConfig where

import Xmobar.Config
import Xmobar.Interfaces


myXmobar = defaultXmobar
  { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
  , additionalFonts = [ "xft:FontAwesome:pixelsize=14:antialias=true:hinting=true" ]
  , bgColor = "black"
  , fgColor = "grey"
  , position = OnScreen 0 Top
  , commands = 
    [ Run StdinReader

    , Run$ Cpu
        [ "-L", "7"
        , "-H", "50"
        , "--normal", "#9F9"
        , "--high", "#F99"
        ] 100

    , Run$ Memory
        ["-t","Mem: <usedratio>%"] 100

    , Run$ Swap [] 100

    , Run$ Battery
        [ "--template" , "<acstatus>"
        , "--"
        , "-o"  , "<fn=1>\xf242</fn> <left>% (<timeleft>)"
        , "-O"  , "<fn=1>\xf0e7</fn> <left>%"
        , "-i"  , "<fn=1>\xf0e7</fn>"
        ] 50

    , Run$ Com "/home/kiren/.xmonad/scripts/wireless.sh" [] "wifi" 30

    , Run$ Com "/home/kiren/.xmonad/scripts/vol_lvl.sh" [] "myvolume" 10

    , Run$ Date "%a %-d/%-m %H:%M:%S" "date" 10

    , Run$ Weather "EGPF"
        [ "-t", " <tempC>°"
        , "-L", "8"
        , "-H", "19"
        , "--high", "#F99"
        , "--low","#99F"
        ] 36000
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "  %StdinReader% }{ <fc=#999>%cpu% | %memory% * %swap%</fc> | %wifi% | %myvolume% | %battery%   %date%%EGPF%  "
  }
