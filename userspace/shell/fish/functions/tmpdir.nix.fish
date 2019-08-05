{ mktemp, fish, rm }: ''
function tmpdir
  set -l tmpDir (${mktemp} --directory)
  pushd $tmpDir
  ${fish}/bin/fish
  popd
  ${rm} -rf $tmpDir
end
''
