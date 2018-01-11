Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , commands = [ Run StdinReader
                    , Run Cpu ["-L","7","-H","50","--normal","#9F9","--high","#F99"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %-d/%-m %H:%M" "date" 10
                    , Run Weather "EGPF" ["-t"," <tempC>°","-L","8","-H","19","--high","#F99","--low","#99F"] 36000
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "  %StdinReader% }{ <fc=#999>%cpu% | %memory% * %swap%</fc>   %date%%EGPF%  "
       }
