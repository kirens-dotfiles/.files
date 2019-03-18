{ bash, feh, findutils, coreutils, betterlockscreen }:
''
#! ${bash}/bin/bash

bg=$(${findutils}/bin/find ${./.} -type f -name '*.jpg' \
   | ${coreutils}/bin/shuf -n 1 \
   )

${feh}/bin/feh --bg-scale "$bg"

${betterlockscreen}/bin/betterlockscreen --update "$bg"
''
