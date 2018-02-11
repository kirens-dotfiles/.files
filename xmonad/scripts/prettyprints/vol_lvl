#! /usr/bin/env bash
vol=$(amixer get Master | grep -oe "[0-9]*%" -m1)
vol="${vol::-1}"

if $(amixer get Master | grep "off" > /dev/null)
then # Mute
    printf "<fn=1>\\uf026</fn>"
elif $(test "$vol" -eq 0)
then # Off
    printf "<fn=1>\\uf026</fn> $vol%%"
elif $(test "$vol" -lt 40)
then # Low
    printf "<fn=1>\\uf027</fn> $vol%%"
else # High
    printf "<fn=1>\\uf028</fn> $vol%%"
fi

