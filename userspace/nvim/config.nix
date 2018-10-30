{ pkgs }:
{
  customRC = import ./rc.nix { };
  packages.myVimPackage = with pkgs.vimPlugins; {
    # loaded on launch
    start = [ vim-closetag ];
    # manually loadable by calling `:packadd $plugin-name`
    opt = [ ];
  };
}

