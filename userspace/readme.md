# Userspace configuration

This is the configuration for my user on my worksations.

## Regular problems

### Wrong hash
I have had references to specific releases of nixpkgs as a fetchUrl call.
I'm unsure as to what exactly happens, but it seems these may get patched or
something else, the result is a mismatch looking like the folowing.
```
unpacking 'https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz'...
error: store path mismatch in file downloaded from 'https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz'
```
The solution is to update the hash in fetchUrl call using the result of
running the `nix-prefetch-url --unpack URL` command.
