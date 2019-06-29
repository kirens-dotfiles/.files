[(self: super: {
  myLib = self.callPackage ./lib { self = self.myLib; };
  gitkraken = super.gitkraken.overrideAttrs (old: {
    installPhase = old.installPhase + ''
      unlink $out/bin/gitkraken
      makeWrapper $out/share/gitkraken/gitkraken $out/bin/gitkraken \
        --set PATH ${self.coreutils}/bin
    '';
  });
  spotify = super.spotify.overrideAttrs (import ./spotify.nix self);
})]
