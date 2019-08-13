# This file contains nixos-specific packages that depend on the configuration
config: self: super: {
  nixos-install = config.system.build.nixos-install;

  nix-build-install = self.callPackage ./nix-build-install { };
}
