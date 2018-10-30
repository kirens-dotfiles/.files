{ alsaUtils }:
{
  executable = true;
  text = ''
    #! /usr/bin/env bash
    ${alsaUtils}/bin/amixer get Master | grep -oe "[0-9]*%" -m1
  '';
}
