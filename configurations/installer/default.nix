{ config, pkgs, ... }:

{
  useEnv = [ ./env.nix ];
  imports = [
    ../../deps/nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix
  ];
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  environment.systemPackages = with pkgs; [
    git networkmanager gparted dotfiles-installer nix-build-install
  ];

  programs.fish.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "sv-latin1";
  };

  services.xserver.enable = true;
  services.xserver.displayManager.startx.enable = true;

  services.openssh.enable = true;
  # Enable SSH in the boot process.
  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
  users.users.root.openssh.authorizedKeys.keys = [
    config.myCfg.installerPublicSSHKey
  ];
  # This is alerady set by installation module
  # services.openssh.permitRootLogin = "yes";

  # Create an iso instead of an installer
  entrypoint = pkgs.iso-writer config.system.build.isoImage;
}
