{ rofi, echo, test, tail, translate }:
''
print_menu() {
  ${echo} "english"
  ${echo} "swedish"
}
option_count=2
continue=0

while ${test} "$continue" -eq 0
do
  language=''$(print_menu | ${rofi} -dmenu -i -lines "$option_count"
  -p 'Language' -no-custom -format i)

  if [ -z "$language" ] ; then
    exit
  fi

  toTranslate=$(${rofi} -dmenu -i -lines 0 -p 'Enter phrase')

  if [ -z "$toTranslate" ] ; then
    exit
  fi

  translation=$(${translate} \
      -engine google \
      -show-original n \
      -show-languages n \
      -show-alternatives n \
      -indent 1 \
      -to "$language" \
      "$toTranslate" \
      | ${tail} -n +6
  )
  ${echo} "$translation" \
    | ${rofi} -dmenu -i -sep '\0' -lines 1 -eh `echo "$translation" | wc -l`
  continue=$?
done
''
