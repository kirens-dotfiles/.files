{ alsaUtils, gnugrep }:
''
#! /usr/bin/env bash
${alsaUtils}/bin/amixer get Master | ${gnugrep}/bin/grep -oe "[0-9]*%" -m1
''
