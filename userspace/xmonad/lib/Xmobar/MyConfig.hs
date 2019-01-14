module Xmobar.MyConfig where

import Xmobar.Config
import Xmobar.Interfaces
import qualified Nix.Vars as Pkgs


barOnScreen screenNr = defaultXmobar
  { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
  , additionalFonts = [ "xft:FontAwesome:pixelsize=14:antialias=true:hinting=true" ]
  , bgColor = "black"
  , fgColor = "grey"
  , position = OnScreen screenNr Top
  , commands =
    [ Run StdinReader

    , Run$ Cpu
        [ "-t", "<user>:<total>%"
        , "-L", "7"
        , "-H", "50"
        , "--normal", "#9F9"
        , "--high", "#F99"
        ] 50

    , Run$ Memory
        ["-t","<usedratio>"] 100 -- Look into coloring percentages

    , Run$ Swap ["-t", "<usedratio>%"] 100

    , Run$ Battery
        [ "--template" , "<acstatus>"
        , "--low", "darkred"
        , "--"
          , "-o"  , "<fn=1>\xf242</fn> <left>% (<timeleft>)"
          , "-O"  , "<fn=1>\xf0e7</fn> <left>%"
          , "-i"  , "<fn=1>\xf0e7</fn>"
        ] 10

    , Run$ Com Pkgs.wirelessScript [] "wifi" 30

    , Run$ Com Pkgs.printVolLvlScript [] "myvolume" 10

    , Run$ Date "%a %-d/%-m %H:%M:%S" "date" 10
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "  %StdinReader% }{ <fc=#999>%cpu% %memory%+%swap% |</fc> %wifi% %myvolume% %battery%   %date%  "
  }

