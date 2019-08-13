{ ... }:
{
  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "ehci_pci"
      "ahci"
      "usbhid"
      "usb_storage"
      "sd_mod"
    ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    cleanTmpDir = true;

    loader = {
      systemd-boot.enable = true;
      #efi.canTouchEfiVariables = true;

      grub.device = "nodev";
      #grub.efiSupport = true;
    };
  };
}
