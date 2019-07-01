{ lib, options, config, ... }:
let
  notSpecified = option: abort ''Missing option!

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
    listOf
    path
    string
    submodule
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

  mergeExistingPaths = lib.concatMap (path:
    if lib.pathExists path
      then [ ({ value = import path { }; }) ]
      else
        lib.warn
          "Ignoring environment-config \"${toString path}\" that is nonexistent"
          []
  );
in {
  options = {
    useEnv = lib.mkOption {
      default = [];
      description = ''
        A path from which environment variables will be loaded
      '';
      type = listOf path;
    };
    myCfg = lib.mkOption {
      default = {};
      description = ''
        Contains properties that should not be version controlled.
      '';
      type = submodule {
        options = lib.mapAttrs mkConfig {
          bootMessage = "Message to be displayed on the grub boot screen";
          mullvadAccountNumber = "Identifier for Mullvad vpn account";
          togglAccessToken = "Access token for your toggl account";
        };
      };
    };
  };
  config = {
    myCfg = let
      envs = mergeExistingPaths config.useEnv;
      merged = lib.mergeDefinitions [ "myCfg" ] options.myCfg.type envs;
    in merged.optionalValue.value or { };
  };
}
