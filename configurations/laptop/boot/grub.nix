{ config, pkgs, lib, ... }:

{
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "usb_storage"
      "sd_mod"
      "rtsx_pci_sdmmc"
    ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    cleanTmpDir = true;

    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = null;
      grub = {
        enable = true;
        version = 2;
        device = "nodev";
        efiSupport = true;
        gfxmodeEfi = "1920x1080";
        splashImage = pkgs.callPackage ./splashImage.nix {
          message = config.myCfg.bootMessage;
        };
      };
    };
  };
}
