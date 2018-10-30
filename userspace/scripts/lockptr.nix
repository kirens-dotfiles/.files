{ pkgs }:
{
  executable = true;
  text = ''
    #! /usr/bin/env bash
    #hhp

    lock () {
        ${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1
    #    ~/.xmonad/scripts/hhp
    }
    unlock () {
        ${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=0
    }

    if ${pkgs.xorg.xf86inputsynaptics}/bin/synclient | grep 'TouchpadOff.*0'; then
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
  '';
}
