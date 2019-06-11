{ lib, ... }:

{
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/ce57694b-5b83-4fb5-abff-c5b4593df273";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "discard" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/B855-B520";
      fsType = "vfat";
    };

  fileSystems."/e" =
    { device = "/dev/disk/by-uuid/89e8debb-f3eb-4f57-99b5-c89845c8c5df";
      fsType = "btrfs";
    };

  fileSystems."/var/lib/docker" = {
    device = "/dev/disk/by-uuid/682557cb-cdc4-4826-b7c1-095806d97d11";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."enc-data-3d".device = "/dev/disk/by-uuid/0e96be52-bed9-4d10-9858-1da9773541da";

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
