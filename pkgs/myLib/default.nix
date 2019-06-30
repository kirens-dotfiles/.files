{ self, lib, callPackage, stdenv, ... }:
{
  bash = callPackage ./bash { self = self.bash; };

  derivationError = msg: stdenv.mkDerivation { name = abort msg; };

  mapLines = mapper: text:
    lib.concatMapStringsSep "\n" mapper (lib.splitString "\n" text);
}
