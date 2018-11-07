module XMonad.Config.Kirens.Keys (
  keyConfig, Actions (..)
) where

-- import Xmonad ( X )
import qualified XMonad.StackSet as StackSet
import XMonad ( X, liftIO, workspaces, Resize (Expand, Shrink), screenWorkspace, ChangeLayout (NextLayout), whenJust )
import XMonad.Operations ( windows, kill, sendMessage )
import Graphics.X11.Types ( KeyMask )
import Data.Default ( Default (def) )
import System.Posix.Process ( executeFile, forkProcess )
import XMonad.Layout.BinarySpacePartition ( ResizeDirectional (ExpandTowards, ShrinkFrom), Direction2D (U, D, L, R), Rotate (Rotate), Swap (Swap) )
import System.Exit ( exitWith, ExitCode (ExitSuccess) )
import XMonad.Actions.SinkAll ( sinkAll )
import XMonad.Actions.CycleWS ( prevWS, nextWS )
import XMonad.Layout.MultiToggle ( Toggle (Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers (FULL) )
import XMonad.Util.EZConfig ( mkKeymap )
import Nix.Vars ( xmessage )

type KeyBnd = String

modNone :: KeyMask
modNone = 0


data Actions =
  Actions
  { menu :: X ()
  , terminalPlain :: X ()
  , editDotfiles :: X ()
  , editTODO :: X ()
  , screenBrightnessInc :: X ()
  , screenBrightnessDec :: X ()
  , volumeInc :: X ()
  , volumeDec :: X ()
  , volumeToggleMute :: X ()
  , trackpadEnabledToggle :: X ()
  , printScreen :: X ()
  , clipboardManager :: X ()
  , lockscreen :: X ()
  , resetScreens :: X ()
  , restartXMonad :: X ()
  }

instance Default Actions where
  def =
    Actions
    { menu =
        noAction "launcher"
    , terminalPlain =
        noAction "plain terminal"
    , editDotfiles =
        noAction "editing dotfiles"
    , editTODO =
        noAction "editing TODO-notes"
    , screenBrightnessInc =
        noAction "increasing screen brightness"
    , screenBrightnessDec =
        noAction "decreasing screen brightness"
    , volumeInc =
        noAction "increasing volume"
    , volumeDec =
        noAction "decreasing volume"
    , volumeToggleMute =
        noAction "muting volume"
    , trackpadEnabledToggle =
        noAction "toggling the trackpad"
    , printScreen =
        noAction "taking screenshots"
    , clipboardManager =
        noAction "clipboard manager"
    , lockscreen =
        noAction "locking the screen"
    , resetScreens =
        noAction "reseting the screens"
    , restartXMonad =
        noAction "restarting XMonad"
    }

-- A default binding when no program action is available
noAction actionName =
  liftIO $ do
    forkProcess
      $ executeFile xmessage True ["-default", "okay", "Action missing for " ++ actionName] Nothing
    return ()

-- This should be made a better generalization (KeyBnd strings error prone) TODO
keysWithPar :: (KeyBnd -> a -> (KeyBnd, X ())) -> [KeyBnd] -> [a] -> [(KeyBnd, X ())]
keysWithPar fn ks as = [ fn k a | (k, a) <- zip ks as ]

numsWith :: KeyBnd -> [KeyBnd]
numsWith k = map ((k++).show) [1..9]

keyConfig actions conf = mkKeymap conf $
  -- Quick launches
  [ ("M-a", menu actions)
  , ("M-s", terminalPlain actions)
  , ("M-e", editDotfiles actions)
  , ("M-t", editTODO actions)

  -- Window mgmt
  , ("M-d", kill)
  , ("M-<Return>", windows StackSet.focusMaster)
  , ("M-j", windows StackSet.focusDown)
  , ("M-k", windows StackSet.focusUp)
  , ("M-S-j", windows StackSet.swapDown)
  , ("M-S-k", windows StackSet.swapUp)
  , ("M-S-<Return>", windows StackSet.swapMaster)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-<Space>", sinkAll)
  -- For BSP Layout
  , ("M-<Up>", sendMessage $ ExpandTowards U)
  , ("M-<Down>", sendMessage $ ExpandTowards D)
  , ("M-<Left>", sendMessage $ ExpandTowards L)
  , ("M-<Right>", sendMessage $ ExpandTowards R)
  , ("M-S-<Up>", sendMessage $ ShrinkFrom U)
  , ("M-S-<Down>", sendMessage $ ShrinkFrom D)
  , ("M-S-<Left>", sendMessage $ ShrinkFrom L)
  , ("M-S-<Right>", sendMessage $ ShrinkFrom R)
  , ("M-<Prior>", sendMessage $ Rotate)
  , ("M-<Next>", sendMessage $ Swap)

  -- Workspaces
  , ("M-C-h", prevWS)
  , ("M-C-l", nextWS)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-f", sendMessage $ Toggle FULL)

  -- Media keys
  , ("<XF86MonBrightnessUp>", screenBrightnessInc actions)
  , ("<XF86MonBrightnessDown>", screenBrightnessDec actions)
  , ("<XF86AudioRaiseVolume>", volumeInc actions)
  , ("<XF86AudioLowerVolume>", volumeDec actions)
  , ("<XF86AudioMute>", volumeToggleMute actions)
  , ("M-p", trackpadEnabledToggle actions)
  , ("<Print>", printScreen actions)

  -- misc
  , ("M-c", clipboardManager actions)
  , ("M-q", lockscreen actions)
  , ("M-M1-S-o", resetScreens actions)
  , ("M-<Esc>", restartXMonad actions)
  , ("M-S-<Esc>", liftIO$exitWith ExitSuccess)
  --, ("M-S-z", spawn "xscreensaver-command -lock")
  ]
  -- Switch workspace
  ++ keysWithPar
       (\bind i -> (bind, windows $ StackSet.greedyView i))
       (numsWith "M-")
       (workspaces conf)
  -- Move window to workspace
  ++ keysWithPar
       (\bind i -> (bind, windows $ StackSet.shift i))
       (numsWith "M-S-")
       (workspaces conf)

  -- Switch monitor
  ++ keysWithPar
       (\bind i -> (bind, screenWorkspace i >>= flip whenJust (windows . (StackSet.view))))
       (numsWith "M-M1-")
       [0..2]
  -- Move window to monitors
  ++ keysWithPar
       (\bind i -> (bind, screenWorkspace i >>= flip whenJust (windows . (StackSet.shift))))
       (numsWith "M-M1-S-")
       [0..2]

  -- `Start` is enough to open dmenu
  --`additionalKeys` [ ((modNone, xK_Super_L), (spawn "dmenu_run")) ]
