{ nix, echo, fish }:
''
function nx-shell
  ${nix}/bin/nix-shell \
    --command "${fish}/bin/fish" $argv

  ${echo} Current shell depth is (shell-lvl)
end
''
