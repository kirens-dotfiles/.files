[(self: super: {
  myLib = self.callPackage ./lib { self = self.myLib; };
})]
