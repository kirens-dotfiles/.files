{ vimUtils, lib }: let
  mkVimPkg = pname: vimUtils.buildVimPluginFrom2Nix {
    inherit pname;
    version = "current";
    src = ./. + ("/" + pname);
  };
  importOrder = [
    "pkg-configs"

    "auto-leave-insert"
    "tabs-spaces"
    "trailing-space"

    "rebinds"
    "style"
    "highlight-bad-space"
  ];
in {
  names = importOrder;
  plugins = lib.listToAttrs (
    map
      (name: { inherit name; value = mkVimPkg name; })
      importOrder
  );
}
