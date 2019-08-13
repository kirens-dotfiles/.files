[(self: super: {
  myLib = self.callPackage ../myLib { self = self.myLib; };

  lib = super.lib // import ../lib {
    super = super.lib;
    self = self.lib;
  };

  gitkraken = super.gitkraken.overrideAttrs (old: {
    installPhase = old.installPhase + ''
      unlink $out/bin/gitkraken
      makeWrapper $out/share/gitkraken/gitkraken $out/bin/gitkraken \
        --set PATH ${self.coreutils}/bin
    '';
  });

  dotfiles-installer = self.callPackage ./dotfiles-installer { };

  home-manager = (import ../../deps/home-manager { pkgs = self; }).home-manager;

  slimThemes = super.slimThemes // self.callPackage ./slimThemes { };

  rofi-toggl = (self.callPackage ./rofi-toggl { }).package;

  docker = super.docker.overrideAttrs (import ./docker self);

  spotify = super.spotify.overrideAttrs (import ./spotify self);

  st = super.st.overrideAttrs (attrs: { src = ./st; });

  git-customized = super.git.overrideAttrs (import ./git self);

  fish = super.fish.overrideAttrs (_: {
    version = "custom";
    src = self.fetchFromGitHub {
      owner = "fish-shell";
      repo = "fish-shell";
      rev = "7ee675afcf486609caf03018b54d1ccd20efec07";
      sha256 = "0fdcxz4z06lmhp2lr9xcl82nxjsb43rv1nq33pdwj5blslbb0g6g";
    };
  });
})]
