{ bash, feh, findutils, coreutils }:
''
#! ${bash}/bin/bash

${feh}/bin/feh --bg-scale \
  $(${findutils}/bin/find ${./.} -type f -name '*.jpg' \
   | ${coreutils}/bin/shuf -n 1 \
   )
''
