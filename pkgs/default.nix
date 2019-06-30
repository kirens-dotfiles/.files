[(self: super: {
  myLib = self.callPackage ./lib { self = self.myLib; };

  lib = let
    superLib = (super.lib or {});
  in superLib // self.callPackage ./lib/lib.nix { super = superLib; };

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
