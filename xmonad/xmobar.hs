Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Weather "KSAT" ["-t","<tempF>F","-L","70","-H","90","--normal","green","--high","red","--low","cyan"] 36000
                    , Run Network "eth0" ["-L","0","-H","32","--normal","darkcyan","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50","--normal","darkcyan","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %Y %l:%M%P" "date" 10
                    , Run Com "/home/cameron/bin/volume.sh" [] "vol" 10
                    , Run StdinReader
                    , Run BatteryP ["BAT0"] ["-t", "<acstatus>" , "-L", "10", "-H", "80" , "-l", "red", "-h", "green" , "--", "-O", "Charging", "-o", "B:<left>%"] 10
                    , Run Com "mail-check" [] "mail" 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %mail% | %cpu% | %memory% * %swap% | %eth0% | <fc=darkcyan>%date%</fc> | %KSAT% | %battery%"
       }
