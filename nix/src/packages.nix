# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ## tools
    wget
    # dig (DNS)
    bind
    # VPN
    #wireguard
    #wireguard-tools

    # btrFS
    btrfsProgs

    wirelesstools
    unzip
    neovim
    gitAndTools.gitFull

    ghc
    haskellPackages.xmobar

    ## X11 stuff
    # Graphical screen config utility
    arandr

    i3lock-fancy
    xautolock
    # Image viewer
    feh
    xclip
    xorg.xwininfo
    xlibs.xbacklight
    # Detect keypress
    xlibs.xev
    # Remaping keys
    xlibs.xmodmap

    # Magnifier
    xzoom

    imagemagick7
    mupdf

    vlc

    dmenu

    # Explorer
    krusader

    gimp
    inkscape
    libreoffice
    thunderbird
    firefox
    chromium
    atom
   
  ];
}