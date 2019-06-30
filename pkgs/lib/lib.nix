{ super, self }:
let lib = self; in
{
  maintainers = (super.maintainers or { }) // {
    kirens = {
      email = "nix@erik.work";
      github = "kirens";
      name = "Erik Nygren";
    };
  };
  licenses = (super.licenses or { }) // {
    unknown = {
      free = false;
      fullName = "License is not known or specified";
      shortName = "unknown";
    };
  };
}
