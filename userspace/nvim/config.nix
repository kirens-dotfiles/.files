{ vimPlugins }:
{
  customRC = import ./rc.nix.vim { };
  vam = {
    knownPlugins = vimPlugins;
    pluginDictionaries = [
      {
        names = [
          "vim-signify"
          "vim-javascript"
          "vim-colors-solarized"
          "ale"
          "vim-localvimrc"
          "vim-nix"
        ];
      }
    ];
  };
}

