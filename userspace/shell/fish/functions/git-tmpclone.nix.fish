{ mktemp, fish, rm, git }: ''
function git-tmpclone
  set -l tmpDir (${mktemp} --directory)
  ${git}/bin/git clone $argv $tmpDir
  pushd $tmpDir
  ${fish}/bin/fish
  popd
  ${rm} -rf $tmpDir
end
''
