module Prgms.ErrorPrompt
  ( noAction
  ) where

import Nix.Vars ( xmessage )
import System.Posix.Process ( executeFile, forkProcess )
import XMonad ( liftIO )

-- A default binding when no program action is available
noAction actionName =
  liftIO $ do
    forkProcess
      $ executeFile xmessage True ["-default", "okay", "Action missing for " ++ actionName] Nothing
    return ()

