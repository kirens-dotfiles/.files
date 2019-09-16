{ makeWrapper, haskellPackages, haskell, writeText }: let
  hsPkgsWithOverridenXMonad = haskellPackages.override {
    overrides = self: super: {
      xmonad = haskell.lib.appendPatch
        super.xmonad ( writeText "patch" ''
          diff --git a/src/XMonad/Main.hs b/src/XMonad/Main.hs
          index 70033a3..6486644 100644
          --- a/src/XMonad/Main.hs
          +++ b/src/XMonad/Main.hs
          @@ -418,7 +421,8 @@ handle event@(PropertyEvent { ev_event_type = t, ev_atom = a })
           handle e@ClientMessageEvent { ev_message_type = mt } = do
               a <- getAtom "XMONAD_RESTART"
               if (mt == a)
          -        then restart "xmonad" True
          +        then io (lookupEnv "XMONAD_BINARY")
          +            >>= flip restart True . fromMaybe "xmonad"
                   else broadcastMessage e

           handle e = broadcastMessage e -- trace (eventName e) -- ignoring
        '');
    };
  };
in hsPkgsWithOverridenXMonad.ghcWithPackages (p: with p; [
  xmonad
  xmonad-contrib
  xmonad-extras
]) // { haskellPackages = hsPkgsWithOverridenXMonad; }
