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

  # TODO: Warn if `name` is an invalid shell variable
  getEnvName = name: "DOTFILES_${name}";

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
  fields = {
    bootMessage = "Message to be displayed on the grub boot screen";
    # This can lead to a lot of trouble, use with care
    dotfilesPath = "Supposed to inform about the config location";
    mullvadAccountNumber = "Identifier for Mullvad vpn account";
    togglAccessToken = "Access token for your toggl account";
    multiroomHost = "Hostname for multiroom device";
  };
  myCfgModule = lib.mkOption {
    default = {};
    description = ''
      Contains properties that should not be version controlled.
    '';
    type = submodule {
      options = lib.mapAttrs (n: _:
        # XXX: Need to make this default more resiliant, but it'll have to work
        #      for now :/
        lib.mkOption (descOrConf n // { default = ""; })
      ) fields;
    };
  };

in {
  options = {
    useEnv = lib.mkOption {
      default = [];
      description = ''
        A path from which environment variables will be loaded
      '';
      type = listOf path;
    };
    myCfg =  lib.mapAttrs mkConfig fields;
  };
  config = {
    myCfg = let
      envs = mergeExistingPaths config.useEnv;
      merged = lib.mergeDefinitions [ "myCfg" ] myCfgModule.type envs;
    in builtins.intersectAttrs fields (
      lib.filterAttrs (_: v: v != "") merged.optionalValue.value or { }
    );
  };
}
