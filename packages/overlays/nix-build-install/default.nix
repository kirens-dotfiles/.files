{ lib, writeShellScriptBin, coreutils, nix, nixos-install }:
writeShellScriptBin "nix-build-install" ''
  PATH="${coreutils}/bin:${nix}/bin"
  echo Warning, not tested since writen!

  build="$1"
  shift
  echo Note: we want the regular system build, with a switch-to-configuration.
  echo To get one you might need to append `.build` to your selection

  mountPoint=/mnt

  # Peek at arguments to get mount-point
  for i in "$@"
  do
  case $last in
      --root)
      mountPoint="$i"
      last=""
      ;;
      *)
      last="$i"
      ;;
  esac
  done

  trap "rm -rf $tmpdir" EXIT
  tmpdir="$(mktemp -d)"

  echo Building system in ./. onto "$mountPoint"
  outLink="$tmpdir/system"
  nix-build -A "$build" --out-link "$outLink" --store "$mountPoint"
  system=$(readlink -f $outLink)

  echo Continuing with nixos-install
  ${nixos-install}/bin/nixos-install \
    --root "$mountPoint" --system "$system" --no-root-passwd "$@"
''
