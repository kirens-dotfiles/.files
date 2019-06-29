{ writeShellScript, coreutils, xdotool, ... }: attrs: let
  wrapper = writeShellScript "spotify-launcher" ''
    win=`${xdotool}/bin/xdotool search --class "spotify" | ${coreutils}/bin/tail -n 1`
    if ${coreutils}/bin/test "$win" != ""
    then
      ${xdotool}/bin/xdotool windowactivate "$win"
    else
      @spotify@
    fi
  '';
in {
  postInstall = ''
    unlink $out/bin/spotify
    install ${wrapper} $out/bin/spotify
    substituteInPlace $out/bin/spotify \
      --subst-var-by spotify $out/share/spotify/spotify
  '';
}
