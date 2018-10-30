{ wirelesstools, grep, awk, coreutils }:
{
  executable = true;
  text = ''
    #! /usr/bin/env bash
    # Based on solution by pbrisbin from Arch Forums
    # https://bbs.archlinux.org/viewtopic.php?pid=575358#p575358

    ${wirelesstools}/bin/iwconfig wlan0 2>&1 | ${grep}/bin/grep -q no\ wireless\ extensions\. && {
      echo wired
      exit 0
    }

    essid=`${wirelesstools}/bin/iwconfig wlo1 | ${awk}/bin/awk -F '"' '/ESSID/ {print $2}'`
    stngth=`${wirelesstools}/bin/iwconfig wlo1 | ${awk}/bin/awk -F '=' '/Quality/ {print $2}' | ${coreutils}/bin/cut -d '/' -f 1`
    bars=`${coreutils}/bin/expr $stngth / 10`

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

    echo $essid $bar

    exit 0
  '';
}
