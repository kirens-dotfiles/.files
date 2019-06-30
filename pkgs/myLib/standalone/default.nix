let
  inherit (builtins)
    all
    attrNames
    isAttrs
    trace
    ;
in rec {
  flip = a: b: c: a c b;
  allOf = flip all;

  /* True if structure isn't a subset of subject

     The function works depth first as we assume structure has a simple
     definition.

     Example:
       missingStructure
         { a = { b = { c = 1; d = 2; } e = 3; }; f = 4; }
         { q = 0; a.b.d = 2; }
       => false

       missingStructure
         { q = 0; a.b.d = 2; }
         { a = { b = { c = 1; d = 2; } e = 3; }; f = 4; }
       => true

     Type:
       set -> set -> bool
  */
  hasStructure = hasPropsMatching (subject: structure:
    if isAttrs structure
      then isAttrs subject && hasStructure subject structure
      else subject == structure
  );

  /* True if subject has all properties of object and thier respective values
     match.
  */
  hasPropsMatching = match: subject: object:
    allOf (attrNames object) (name:
      subject ? ${name} && match subject.${name} object.${name}
    );
}
