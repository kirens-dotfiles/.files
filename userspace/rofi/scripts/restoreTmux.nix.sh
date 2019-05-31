{ bash, rofi, tmux, grep, st, test }:
''
#! ${bash}
res=$(${tmux} ls \
      | ${grep} --invert-match attached \
      | ${rofi} -dmenu -p 'Unattached sessions' \
      | ${grep} -o -P '^.*?(?=:)' \
     )

if ${test} -z "$res"
then
    exit
fi

${st} -e ${tmux} a -t "$res"
''
