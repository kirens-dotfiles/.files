{ super, ... }:
{
  maintainers = super.maintainers // {
    kirens = {
      email = "nix@erik.work";
      github = "kirens";
      name = "Erik Nygren";
    };
  };
  licenses = super.licenses // {
    unknown = {
      free = false;
      fullName = "License is not known or specified";
      shortName = "unknown";
    };
  };
}
