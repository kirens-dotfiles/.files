{ vimPlugins, callPackage }: let
  myModules = callPackage ./modules { };
in {
  customRC = ''
    " Default to X clipboard
    set clipboard=unnamedplus

    " map mouse activity
    set mouse=a

    set virtualedit=onemore

    set undofile
  '';
  vam = {
    knownPlugins = vimPlugins // myModules.plugins;
    pluginDictionaries = [
      {
        names = [
          "ale"
          "csv-vim"
          "rust-vim"
          "typescript-vim"
          "vim-colors-solarized"
          "vim-fish"
          "vim-fugitive"
          "vim-go"
          "vim-javascript"
          "vim-lastplace"
          "vim-localvimrc"
          "vim-nix"
          "vim-signify"
          "vim-toml"
        ];
      }
      { inherit (myModules) names; }
    ];
  };
}

