{ lib, writeShellScriptBin, coreutils, utillinux }: iso:
writeShellScriptBin "iso-writer" ''
  PATH="${coreutils}/bin:${utillinux}/bin"
  fail() {
    code="$1"
    shift
    echo "$@" >&2
    exit "$code"
  }

  device="$1"

  if ! devSize=`lsblk -o size "$device" --nodeps --noheadings -b`
  then
    fail 1 Could not identify device
  fi

  iso="${iso}/iso/${iso.name}"

  if ! test -f "$iso"
  then
    fail 1 Could not find iso in '"${iso}"'
  fi

  if test "$devSize" -lt "$(du "$iso" | cut -f1)"
  then
    fail 1 Iso file will not fit on medium
  fi

  read -t 30 -p "Overwrite $device with ISO? (y/N) " answer
  case $answer in
  Y|y|yes)
    echo Will proced to write to device
    ;;
  *)
    fail 0 No user confirmation recived
    ;;
  esac

  dd bs=4M if="$iso" of="$device" conv=fdatasync  status=progress
''
