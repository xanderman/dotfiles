Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = BottomW L 90
       , lowerOnStart = False
       , commands = [ Run Weather "KBJC" ["-t","<tempF>F <skyCondition>","-L","32","-H","70","--normal","green","--high","red","--low","lightblue"] 18000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %k:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% } %cpu% | %memory% * %swap% { <fc=#ee9a00>%date%</fc> | %KBJC%"
       }
