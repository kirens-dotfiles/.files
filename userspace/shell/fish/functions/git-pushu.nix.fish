{ git, gnugrep }:
let
  git' = "${git}/bin/git";
  grep = "${gnugrep}/bin/grep";
in
''
function git-pushu
  ${git'} push --set-upstream $argv[1] (${git'} branch | ${grep} -Po '(?<=\* ).+')
end
''
