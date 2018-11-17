{
  allowUnfreePredicate = (pkg:
    builtins.elem
      (builtins.parseDrvName pkg.name).name
      [
        "spotify"
        "gitkraken"
      ]
  );
}
