{ lib, ... }:
let
  notSpecified = option: abort ''

    Secret option `${option}` not specified.
    Do so by setting the "${getEnvName option}" environment variable or adding
    the following to your configuration
      myCfg.${option} = {{some value}};
    It is reccomended that these options are kept in a file that is not tracked
    by version control.
  '';

  getEnvName = name: "cfgEnv-${name}";

  envOrUnspecified = name: let
    value = builtins.getEnv (getEnvName name);
  in
    if value == ""
      then notSpecified name
      else value
    ;

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
    { default = envOrUnspecified name; }
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
