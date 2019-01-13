{ stdenv, writeTextFile }: commands:
let
  installScript = package:
    writeTextFile {
      name = builtins.concatStringsSep "-"
        [ package.name "restricted" "installer" ];

      text = builtins.concatStringsSep "\n" (
        [ "mkdir -p $out/bin/" ] ++
        map
          (mapping:
            let
              from = mapping.from or mapping;
              to = mapping.to or from;
              canThrow = mapping.canThrow or true;
            in ''
              if [ -e "${from}" ]; then
                mkdir -p ''$(dirname $out/${to})
                ln -s ${package.outPath}/${from} $out/${to}
                echo "[Linked file] \"${to}\" from \"''$(ls -ld ${from})\""
              else
                (>&2 echo 'Could not find file "${from}"')
                ${if canThrow then "exit 1" else ""}
              fi
            '')
          commands
      );
    };

  restricter = package:
    stdenv.mkDerivation {
      name = builtins.concatStringsSep "-" [ package.name "restricted" ];
      src = package.outPath;
      installPhase = "bash ${installScript package}";
    };
in package:
  restricter package // {
    override = newArgs: restricter ( package.override newArgs );
  }
