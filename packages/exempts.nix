let
  exemptPkgs = [
    { name = "spotify"; }
    { name = "gitkraken"; }
  ];
  exemptLicenses = [
    { spdxId = "CC-BY-NC-3.0"; }
    { shortName = "unknown"; }
  ];

  inherit (builtins) parseDrvName trace any;
  inherit (import ./myLib/standalone) hasStructure;

  unknown = { unknown = "unknown"; };

  okPkg = attr: any (hasStructure attr);

in {
  allowUnfreePredicate = pkg: let
    name = if pkg ? name then (parseDrvName pkg.name) else unknown;
    nameText = name.name or "?";
    license = pkg.meta.license or unknown;
    licenseText = license.fullName or license.shortName or "?";
  in if okPkg name exemptPkgs || okPkg license exemptLicenses
    then trace "Exempting unfree licensed: ${nameText} (${licenseText})" true
    else false
    ;
}
