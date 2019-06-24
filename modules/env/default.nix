{ lib, ... }:
let
  notSpecified = option: abort ''

    Secret option `${option}` not specified.
    Do so by adding the following to your configuration
      myCfg.${option} = {{some value}};
    It is reccomended that these options are kept in a file that is not tracked
    by version control.
  '';

  inherit (lib.types)
    submodule
    string
    ;

  makeStringConf = description: { inherit description; type = string; };
  descOrConf = value:
    if lib.isString value
    then makeStringConf value
    else value;
  mkConfig = name: additionalConf: lib.mkOption (
    { default = notSpecified name; }
    //
    (descOrConf additionalConf)
  );
in {
  options = {
    myCfg = lib.mkOption {
      default = {};
      description = ''
        Contains properties that should not be version controlled.
      '';
      type = submodule {
        options = lib.mapAttrs mkConfig {
          bootMessage = "Message to be displayed on the grub boot screen";
          mullvadAccountNumber = "Identifier for Mullvad vpn account";
        };
      };
    };
  };
}
