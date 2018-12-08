function nx-run
  nix-shell -p $argv[1] --command $argv[1]
end
