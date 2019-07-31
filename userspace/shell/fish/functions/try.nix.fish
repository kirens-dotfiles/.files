{ nix, tail }:
''
function try
  if set -l out (${nix}/bin/nix-build '<nixpkgs>' -A $argv[1] --no-out-link)
    set path (echo $out | ${tail} -n 1)
    set PATH "$path/bin" $PATH
    set -x IN_NIX_SHELL try
    fish
  else
    set -l lastStatus $status
    echo out
    return $lastStatus
  end
end
''
