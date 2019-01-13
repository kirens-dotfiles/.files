{ coreutils, which }:
''
function prgm -d lookingFor
  ${coreutils}/bin/readlink --canonicalize (${which}/bin/which $argv[1])
end
''
