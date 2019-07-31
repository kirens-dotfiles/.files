{ vimPlugins, callPackage }: let
  myModules = callPackage ./modules { };
in {
  customRC = ''
    " Default to X clipboard
    set clipboard=unnamedplus

    " map mouse activity
    set mouse=a

    set virtualedit=onemore
  '';
  vam = {
    knownPlugins = vimPlugins // myModules.plugins;
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
      { inherit (myModules) names; }
    ];
  };
}

