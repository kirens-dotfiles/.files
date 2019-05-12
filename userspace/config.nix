let
  inherit (builtins)
    parseDrvName
    elem
    trace
    ;
in {
  allowUnfreePredicate = (pkg:
    let
      unknown = "unknown";
      pkgName = pkg.name or unknown;
      name = if pkgName != unknown
        then (parseDrvName pkgName).name
        else unknown
        ;

      pkgLicense =
        pkg.meta.license.spdxId
        or pkg.meta.license.fullName
        or unknown;

      exemptPkgs = [
        "spotify"
        "gitkraken"
      ];
      exemptLicenses = [
        "CC-BY-NC-3.0"
      ];

      prompt = exempted:
        if exempted
        then trace "Exempting unfree licensed: ${name} (${pkgLicense})" true
        else false
        ;

    in prompt (
      elem name exemptPkgs
      ||
      elem pkgLicense exemptLicenses
    )
  );
}
