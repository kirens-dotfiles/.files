{ xorg, gnugrep }:
''
#! /usr/bin/env bash
#hhp

lock () {
    ${xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1
#    ~/.xmonad/scripts/hhp
}
unlock () {
    ${xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=0
}

if ${xorg.xf86inputsynaptics}/bin/synclient | ${gnugrep}/bin/grep 'TouchpadOff.*0'; then
    # Touchpad is off
    lock
else
    # Touchpad is on
    unlock
fi

#if [ -z $1 ]; then
#    synclient TouchpadOff=1
#    ~/.xmonad/lib/hhp
#else
    # Enable
#    synclient TouchpadOff=0
#fi
''
