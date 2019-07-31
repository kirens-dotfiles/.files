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

  home-manager = (import ../../deps/home-manager { pkgs = self; }).home-manager;

  slimThemes = super.slimThemes // self.callPackage ./slimThemes { };

  docker = super.docker.overrideAttrs (import ./docker self);

  spotify = super.spotify.overrideAttrs (import ./spotify self);

  git-customized = super.git.overrideAttrs (import ./git self);
})]
