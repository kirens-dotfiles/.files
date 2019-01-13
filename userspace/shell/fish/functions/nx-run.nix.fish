{ nix }:
''
function nx-run
  ${nix}/bin/nix-shell -p $argv[1] --command $argv[1]
end
''
