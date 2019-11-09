{ writeScript, python3, bash, coreutils, ... }: { installPhase, ... }: let
  dockerTags = writeScript "dockerTags" ''
    #! ${python3}/bin/python
    ${builtins.readFile ./dockertags.py}
  '';
  dockerWrapper = writeScript "dockerWrapper" ''
    #! ${bash}/bin/bash
    if ${coreutils}/bin/test "$1" == "tags"
    then
      shift
      exec ${dockerTags} "$@"
    fi

  '';
in {
  installPhase = installPhase + ''
    dockerFile=`tail -n +2 $out/bin/docker`

    install -v -D -m755 ${dockerWrapper} $out/bin/docker

    echo "$dockerFile" >> $out/bin/docker
  '';
}
