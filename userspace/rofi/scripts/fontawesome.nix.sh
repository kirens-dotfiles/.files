{ fetchFromGitHub, printf, rofi, cat, cut, xclip }: let
  rofi-fontawesome = fetchFromGitHub {
    owner = "wstam88";
    repo = "rofi-fontawesome";
    rev = "986bff4750fbeb30af0590ed980337c391972ba7";
    sha256 = "1ddmnp6ala9kzg6f6zf6mrvcd6b3s100dzd2h86ss8yn10n10zh5";
  };
in ''
case "$(${printf} 'unicode\nclass' | ${rofi} -dmenu -p 'FontAwesome')" in
  class) listName=icon-list.txt ;;
  *) listName=unicode-list.txt ;;
esac

${cat} ${rofi-fontawesome}/$listName \
  | ${rofi} -dmenu -markup-rows -columns 6 -p 'FontAwesome' \
  | ${cut} -d\' -f2 \
  | ${xclip} -selection clipboard
''
