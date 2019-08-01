let
  inherit (builtins)
    all
    attrNames
    isAttrs
    trace
    foldl'
    pathExists
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

  foldSet = folder: initial: set:
    foldl' (acc: n: folder acc n (set.${n})) initial (attrNames set);

  derivationError = msg: { type = abort msg; };

  tryImport = fallback: path:
    if pathExists path
      then import path
      else
        trace
          "NOTE: Ignoring import \"${toString path}\" that is nonexistent"
          fallback
    ;

  tryImportFn = fallback: tryImport (_: fallback);
}
