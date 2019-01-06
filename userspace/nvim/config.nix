{ pkgs }:
{
  customRC = import ./rc.nix.vim { };
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

