{ lib, self, myLib, coreutils, ... }:
with self; {
  echo = "${coreutils}/bin/echo";

  echoBlock = myLib.mapLines (line: "${echo} ${lib.escapeShellArg line}");

  case = let
    mapCaseList' = mapper: lib.foldl ({ result, prop }: val:
      if isNull prop
      then { inherit result; prop = val; }
      else { result = result ++ [(mapper prop val)]; prop = null; }
    ) { result = [ ]; prop = null; };
    mapCaseList = mapper: list: (mapCaseList' mapper list).result;
    caseToString = case: body: ''
      ${case})
        ${body}
        ;;
    '';
  in on: cases: ''
    case ${on} in
        ${lib.concatStringsSep "\n" (mapCaseList caseToString cases)}
    esac
  '';
}
