# .files
The goal is to have one nix expression for my complete workstation stack,
ensuring reproducible builds.

### Table of contents
  * [Principles](#principles)
    * [Usable](#usable)
    * [Reproducible](#reproducible)
    * [Automated](#automated)
    * [Secure](#secure)
  * [Building / Updating](#building--updating)
  * [Structure](#structure)
  * [Installation](#installation)
    * [Setup disks](#setup-disks)
      * [Initiate the encryption](#initiate-the-encryption)
      * [Open existing disks](#open-existing-disks)
    * [Install NixOS](#install-nixos)

# Principles
### Usable
_Learn by doing - implement first optimize later_

Working code is generally better than no code.
I want to have a general and clean code base, but not at the expense of getting stuck.
I think better patterns to design things will emerge out of the misstakes I come across.

### Reproducible
_I will minimize global state and expose nothing, not even coreutils, in the path by default_

As previously mentioned the aim is for this repo to contain a complete description for all software I use.
If done properly it will make this configuration applicable regardless of time, location or device.

Another part in having reproducible builds is to have a functional mindset.
An ever mutating state as a workstation environment is hard to keep track of.
It might be quicker and easier in the short term to configure things imperatively,
but that results in technical debt I don't want to carry.
Often reinstalling the system is a good way to enforce this as it'll expose flawed installs.

### Automated
_Given a partitioning table the build should be completed by executing a single command_

Configuring should not be cumbersome as this discourages changes.
The software should be suited for my needs and will therefore have to change as I do.
The aim is to have one click installs.

One long term goal is to use CI tools to automatically produce an ISO.
This may at any point be retrived, burned to a USB and with minimal effort used to reinstall the system on a new device.

### Secure
_Minimize attack surface by having a [suckless](https://suckless.org/philosophy/) mentality_

There's a lot more to security and I'm no expert so I don't want to state anything untrue.
However I think part of this structured aproach to system configuration is to perform some kind of threat modeling.
Minimizing attack surface by using nix to determine closures and decrease them might be one good strategy in general.
I hope to end up with a more secure workstation as I, more easily, can vet all software being installed.


# Building / Updating
This whole repo a single nix-expression.
As such the `/default.nix` will result in a build script for the system.

More specifically the result of the expression is actually an attribute set
for each device in my configuration. Running `nix-build -A device` in the
project root will thus create a `./result` that is the activation script for
`device`.

The activation script is run with one argument that determines what kind of
activation that is done.

To test the config in one go run;
```
nix-build -A laptop; and sudo ./result test;
```


# Structure
This repo aims to contain the single nix expression to contain all my config which is located in `default.nix`.

To increase code sharing I aim to utilize modules and what I call _"collections"_.

TODO: Describe this more thoroughly

* **deps**
  * home-manager
  * nixpkgs
* **configuration**  
  Contains a directory with the build expression for each device
  * {{config name}}  
    The structure under these is not yet clear
* **modules**  
  Modules which are specific to my config
* **tests**  
  Tests to verify the derivation, hopefully at some point to test that habits and assumptions of my usage are tested for breakage with a new installation
* **pkgs**
  * overlays  
    Temporary overlays or overlays that might not fit into the nixpkgs
  * lib  
    Slight or temporary modifications to nixpkgs.lib
  * myLib  
    Helper functions that I create
* **collections**
  Supposed to generalize certain concepts such that code sharing between my configurations can increase


# Installation
TODO: Will be updated on next install to reflect changes in nix-setup.

These are the steps I used last time I installed NixOS on my laptop,
heavily inspired by [this](https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134) guide.

## Setup disks
For the install I want an encrypted partition for the OS,
and another one for a data-partition that can be shared between OSs.
The boot partition cannot be encrypted.

All in all we need 3 partitions. I create them with GParted.
First the boot partition (/boot) 500MiB FAT16,
followed by the root partition (/) >40GiB,
and lastly a data partition (/e) >25GiB

### Initiate the encryption
Find the partitions to work with

    lsblk -o name,partlabel

Initiate disk encryption for the devices

    cryptsetup luksFormat /dev/sda2
    cryptsetup luksFormat /dev/sda3

Generate the physical volumes

    cryptsetup luksOpen /dev/sda2 enc-nix
    pvcreate /dev/mapper/enc-nix
    cryptsetup luksOpen /dev/sda3 enc-data

Setup the data-partition, by resizing it to the complete parition size.
Create a volume group of logical volumes, root + swap and data

    cryptsetup resize enc-data
    vgcreate sysg /dev/mapper/enc-nix
    lvcreate -L 8G -n swap sysg
    lvcreate -l '100%FREE' -n root sysg

Format the encrypted volumes

    mkfs.ext4 -L data /dev/mapper/enc-data
    mkfs.ext4 -L root /dev/sysg/root
    mkswap -L swap /dev/sysg/swap

### Open existing disks

Decrypt the disks

    cryptsetup luksOpen /dev/sda2 enc-nix
    cryptsetup luksOpen /dev/sda3 enc-data

Find the volume group

    vgscan

Activate the volumes and make sure they appear in the listing

    lvchange -ay sysg
    lvs

## Install NixOS

Mount newly created partitions so the OS can be installed on them

    mount /dev/sysg/root /mnt
    mkdir /mnt/boot
    mkdir /mnt/e
    mount /dev/sda5 /mnt/boot
    mount /dev/mapper/enc-data /mnt/e
    swapon /dev/sysg/swap

Generate a nixos hardware-config

    nixos-generate-config --root /mnt

We need to fix some things for the config to work well.
Open `/mnt/etc/nixos/hardware-config.nix` and modify the following

* In `fileSystems."/"` add the field `options = [ "noatime" "nodiratime" "discard" ];`
* With the uuid of /dev/sda2 we add the following
```
  boot.initrd.luks.devices."root" = {
    device = "{{uuid}}";
    preLVM = true;
    allowDiscards = true;
  };
```
* Add
```
  boot.loader = {
    grub = {
      enable = true;
      version = 2;
      device = "nodev";
      efiSupport = true;
      gfxmodeEfi = "1024x768";
    };
    efi.canTouchEfiVariables = true;
  };
```

Connect to WiFi. Clone the nix-config and stuff...
install and reboot

    nixos-install
    reboot
