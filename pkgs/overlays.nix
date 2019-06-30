[(self: super: {
  myLib = self.callPackage ./myLib { self = self.myLib; };

  lib = super.lib // import ./lib {
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

  slimThemes = super.slimThemes // self.callPackage ./slimThemes.nix { };

  spotify = super.spotify.overrideAttrs (import ./spotify.nix self);
})]
