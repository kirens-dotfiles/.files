{ self, lib, callPackage, stdenv, ... }:
import ./standalone // {
  bash = callPackage ./bash { self = self.bash; };

  mapLines = mapper: text:
    lib.concatMapStringsSep "\n" mapper (lib.splitString "\n" text);
}
