module Prgms.ClipboardManager
  ( ClipboardManager(..)
  ) where

import Prgms.ErrorPrompt ( noAction )
import Prelude ( (++), ($) )
import XMonad ( X, spawn )
import Data.Default ( Default ( def ) )

data ClipboardManager =
  ClipboardManager
    { init :: X ()
    , toggleShow :: X ()
    , next :: X ()
    , prev :: X ()
    , remove :: X ()
    }

instance Default ClipboardManager where
  def =
    ClipboardManager
      { init = noAction "initiating Clipboard Manager"
      , toggleShow = noAction "opening Clipboard Manager"
      , next = noAction "traversing clipboard history"
      , prev = noAction "traversing clipboard history"
      , remove = noAction "removing clipboard history entries"
      }

