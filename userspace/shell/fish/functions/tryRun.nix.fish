{ nix, tail }:
''
function tryRun
  set -l bin $argv[1]
  set -l attr $argv[1]
  set -l args $argv[2..-1]

  switch $argv[1]
    case --bin
      set bin $argv[2]
      set attr $argv[3]
      set -l args $argv[4..-1]
  end

  if set -l out (${nix}/bin/nix-build '<nixpkgs>' -A $attr --no-out-link)
    set path (echo $out | ${tail} -n 1)
    eval "$path/bin/$bin" $args
  else
    set -l lastStatus $status
    echo out
    return $lastStatus
  end
end
''
