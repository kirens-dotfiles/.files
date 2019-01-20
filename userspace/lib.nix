{ lib }:

lib.fix (self: {
  removeUnwantedArgs = f: args:
    builtins.intersectAttrs
      (builtins.functionArgs f)
      args;

  callOnlyNessecary = f: args:
    f (self.removeUnwantedArgs f args);

  importWith = path:
    self.callOnlyNessecary (import path);
})
