{ coreutils, vim }:
''
function __fish_editCmd
  set -l tmpBuffer (${coreutils}/bin/mktemp)
  commandline -b > $tmpBuffer
  ${vim} -c set\ ft=fish $tmpBuffer
  commandline -r (${coreutils}/bin/cat $tmpBuffer)
  ${coreutils}/bin/rm $tmpBuffer
end

bind \ce __fish_editCmd
''
