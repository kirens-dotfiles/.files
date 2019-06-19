{ config, pkgs, lib, ... }:
let
  myPkgs = import ../nixpkgs { };

  inherit (lib)
    concatStrings
    mapAttrsToList
    concatMapStrings
    ;

  rejects = concatMapStrings (font: ''
    <pattern>
      <patelt name="family">
        <string>${font}</string>
      </patelt>
    </pattern>
  '');

  rejectFonts = fonts: ''
    <selectfont>
      <rejectfont>
        ${rejects fonts}
      </rejectfont>
    </selectfont>
  '';
  fallbacksFonts = otherFonts: concatStrings (
    mapAttrsToList (name: fonts: ''
      <alias>
        <family>${name}</family>
        ${concatStrings (map (font:
          "<prefer><family>${font}</family></prefer>"
        ) fonts)
        }
      </alias>
    '')
    otherFonts
  );

  fontConfig = content: ''
    <?xml version='1.0'?>
    <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
    <fontconfig>
      ${concatStrings content}
    </fontconfig>
  '';
in {
  fonts = {
    enableGhostscriptFonts = false;
    enableDefaultFonts = false;

    fonts = with pkgs; [
      font-awesome-ttf
      myPkgs.xkcd-font
      twemoji-color-font

      # Basically DejaVu without uneccesary glyphs
      ttf_bitstream_vera

      # Coding
      hack-font
    ];
    fontconfig = {
      enable = true;

      localConf = fontConfig [
        (rejectFonts [
          "DejaVu Sans"
          "DejaVu Serif"
          "DejaVu Sans Mono"
          "EmojiOne Mozilla"
          "Twemoji Mozilla"
        ])
        (fallbacksFonts rec {
          monospace = [ "Source Code Pro" "emoji" ];
          mono = monospace;
          sans-serif = [ "Bitstream Vera Sans" "emoji" ];
          sans = sans-serif;
          serif = [ "Bitstream Vera Serif" "emoji" ];
          cursive = [ "xkcd" "xkcd script" "serif" ];
          fantasy = [];
          system-ui = [ "FontAwesome" ];
          math = [ "FreeSerif" ];
          emoji = [ "Twitter Color Emoji" ];
        })
      ];
    };
  };
}
