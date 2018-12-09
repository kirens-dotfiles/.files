{ pkgs }:
{
  customRC = import ./rc.nix { };
  vam = {
    knownPlugins = pkgs.vimPlugins;
    pluginDictionaries = [
      {
        names = [
          "vim-javascript"
          "vim-colors-solarized"
        ];
      }
    ];
  };
}

