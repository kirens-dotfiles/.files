{ pkgs, ... }:
{
  # It's a me
  users.extraGroups.kiren.gid = 1000;
  users.extraUsers.kiren = {
    isNormalUser = true;
    description = "Erik Nygren";
    group = "kiren";
    extraGroups = [
      "wheel"
      "docker"
    ];
    shell = pkgs.fish;
  };

  # Home configuration
  home-manager.users.kiren = import ./home.nix;
}
