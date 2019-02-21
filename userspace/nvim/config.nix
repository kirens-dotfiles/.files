{ vimPlugins }:
{
  customRC = import ./rc.nix.vim { };
  vam = {
    knownPlugins = vimPlugins;
    pluginDictionaries = [
      {
        names = [
          "vim-javascript"
          "vim-colors-solarized"
          "ale"
        ];
      }
    ];
  };
}

