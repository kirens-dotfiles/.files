{ wirelesstools, bash, gnugrep, gawk, coreutils }:
let
  iwconfig = "${wirelesstools}/bin/iwconfig";
  grep = "${gnugrep}/bin/grep";
  awk = "${gawk}/bin/awk";
  cut = "${coreutils}/bin/cut";
  expr = "${coreutils}/bin/expr";
  echo = "${coreutils}/bin/echo";
in ''
#! ${bash}/bin/bash
# Based on solution by pbrisbin from Arch Forums
# https://bbs.archlinux.org/viewtopic.php?pid=575358#p575358

${iwconfig} wlan0 2>&1 | ${grep} -q no\ wireless\ extensions\. && {
  ${echo} wired
  exit 0
}

essid=`${iwconfig} wlo1 | ${awk} -F '"' '/ESSID/ {print $2}'`
stngth=`${iwconfig} wlo1 | ${awk} -F '=' '/Quality/ {print $2}' | ${cut} -d '/' -f 1`
bars=`${expr} $stngth / 10`

case $bars in
  0)  bar='[----------]' ;;
  1)  bar='[/---------]' ;;
  2)  bar='[//--------]' ;;
  3)  bar='[///-------]' ;;
  4)  bar='[////------]' ;;
  5)  bar='[/////-----]' ;;
  6)  bar='[//////----]' ;;
  7)  bar='[///////---]' ;;
  8)  bar='[////////--]' ;;
  9)  bar='[/////////-]' ;;
  10) bar='[//////////]' ;;
  *)  bar='[----!!----]' ;;
esac

${echo} $essid $bar

exit 0
''
