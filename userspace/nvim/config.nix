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
          "vim-go"
          "vim-colors-solarized"
          "ale"
          "vim-localvimrc"
          "vim-nix"
        ];
      }
    ];
  };
}

