function nx-shell
  nix-shell --command fish $argv
  echo Current shell depth is $SHLVL
end
