{ pkgs, config, ... }:
let
  spoofedShell = pkgs.stdenv.mkDerivation {
    name = "spoofed-shell";
    src = let
      cmd = config.services.xserver.displayManager.session.wrapper;
    in with pkgs; writeTextDir "shell.c" ''
      #include <unistd.h>
      #include <string.h>

      int main(int argc, char* argv[])
        {
          if (strcmp(argv[2], "${cmd}") == 0) execv("${bash}/bin/bash", argv);
          else execl("${sl}/bin/sl", "sl", NULL);
        }
    '';
    buildPhase = "gcc -o shell shell.c";
    installPhase = ''
      mkdir -p $out/bin
      cp shell $out/bin
    '';
    passthru = {
       shellPath = "/bin/shell";
    };
  };
in {
  users.extraGroups.guest.gid = 1001;
  users.extraUsers.guest = {
    isNormalUser = true;
    description = "Guest Guestson";
    group = "guest";
    shell = spoofedShell;
    password = "guest";
  };
  home-manager.users.guest = { pkgs, ... }: {
    xsession = {
      enable = true;
      windowManager.command = "${pkgs.firefox}/bin/firefox";
    };
  };
}
