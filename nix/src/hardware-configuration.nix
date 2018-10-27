# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub =
    { enable = true;
      version = 2;
      device = "nodev"; 
      efiSupport = true;
      gfxmodeEfi = "1920x1080";
    };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/5a957040-728e-4507-a5a3-d2a1bafc9192";
      fsType = "ext4";
      options = [ "noatime" "nodiratime" "discard" ];
    };

  boot.initrd.luks.devices."enc-nix".device = "/dev/disk/by-uuid/06f36ccf-f28b-4939-9313-a2f22d4d8712";
  boot.initrd.luks.devices."enc-data".device = "/dev/disk/by-uuid/0e96be52-bed9-4d10-9858-1da9773541da";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0415-59A5";
      fsType = "vfat";
    };

  fileSystems."/e" =
    { device = "/dev/disk/by-uuid/89e8debb-f3eb-4f57-99b5-c89845c8c5df";
      fsType = "btrfs";
      options = [ "discard" ];
    };

  swapDevices =
    [ { device = "/media/swap"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
