module KeyUp where

import Data.Monoid
import qualified Data.Map as M

import XMonad
import Control.Monad

keyUpEventHook :: Event -> X All
keyUpEventHook e = handle e >> return (All True)

keyUpKeys (XConf{ config = XConfig {XMonad.modMask = modMask} }) = M.fromList $ 
    [ ((modMask, xK_v), io (print "Hi")) ]

handle :: Event -> X ()
handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyRelease = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        ks <- asks keyUpKeys
        userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handle _ = return ()
