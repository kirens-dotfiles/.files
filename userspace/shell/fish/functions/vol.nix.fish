{ alsaUtils }:
''
function vol
  ${alsaUtils}/bin/amixer set Master $argv%
end
''
