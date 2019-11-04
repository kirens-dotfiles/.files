{-# LANGUAGE RebindableSyntax #-}
module XMonad.Config.Kirens.Keys ( normal, modes, Mode (..), use, Actions (..)
) where

-- import Xmonad ( X )
import qualified XMonad.StackSet as StackSet
import XMonad ( X
              , liftIO
              , workspaces
              , Resize (Expand, Shrink)
              , screenWorkspace
              , ChangeLayout (NextLayout)
              , whenJust
              , modMask
              , xfork
              , io
              , ask
              , display
              , theRoot
              , XConf ( XConf )
              , XConfig
              , Layout
              , asks
              , keyActions
              , ungrabPointer
              , ungrabKeyboard
              , currentTime
              , grabKeyboard
              , grabModeAsync
              , grabModeSync
              , grabPointer
              , buttonPressMask
              , none
              , Display

              , (.|.)
              )
import Prelude hiding ((>>))
import qualified Prelude as P
import XMonad.Actions.ModalKeyActions ( Id
                                      , KeyMap
                                      , Name
                                      , normalMode
                                      , useMode
                                      )

import XMonad.Operations ( windows, kill, sendMessage )
import Graphics.X11.Types ( KeyMask )
import Data.Default ( Default (def) )
import XMonad.Layout.BinarySpacePartition ( ResizeDirectional (ExpandTowards, ShrinkFrom), Direction2D (U, D, L, R), Rotate (Rotate), Swap (Swap) )
import System.Exit ( exitWith, ExitCode (ExitSuccess) )
import XMonad.Actions.SinkAll ( sinkAll )
import XMonad.Actions.CycleWS ( prevWS, nextWS )
import XMonad.Layout.MultiToggle ( Toggle (Toggle) )
import XMonad.Layout.MultiToggle.Instances ( StdTransformers (FULL) )
import XMonad.Util.EZConfig ( mkKeymap )
import qualified Prgms.ClipboardManager as CM
import Prgms.ErrorPrompt ( noAction )
import XMonad.Actions.CycleRecentWS ( cycleRecentWS )
import Graphics.X11.Types ( KeySym, KeyCode )
import qualified Graphics.X11.Types as Keys
import Data.Map ( fromList )
import qualified XMonad.Actions.Submap as SM



type KeyBnd = String

modNone :: KeyMask
modNone = 0


data Actions =
  Actions
  { menu :: X ()
  , selectWorkspace :: X String
  , terminalPlain :: X ()
  , volumeInc :: X ()
  , volumeDec :: X ()
  , volumeToggleMute :: X ()
  , multiroomInc :: X ()
  , multiroomDec :: X ()
  , multiroomToggleMute :: X ()
  , trackpadEnabledToggle :: X ()
  , printScreen :: X ()
  , clipboardManager :: CM.ClipboardManager
  , lockscreen :: X ()
  , resetScreens :: X ()
  , restartXMonad :: X ()
  , musicPlayPause :: X ()
  , musicNext :: X ()
  , musicPrev :: X ()
  , defaultKeyboardLayout :: X ()
  }

instance Default Actions where
  def =
    Actions
    { menu =
        noAction "launcher"
    , selectWorkspace =
        noAction "selecting workspaces" P.>> return ""
    , terminalPlain =
        noAction "plain terminal"
    , volumeInc =
        noAction "increasing volume"
    , volumeDec =
        noAction "decreasing volume"
    , volumeToggleMute =
        noAction "muting volume"
    , multiroomInc =
        noAction "increasing multiroom volume"
    , multiroomDec =
        noAction "decreasing multiroom volume"
    , multiroomToggleMute =
        noAction "muting multiroom volume"
    , trackpadEnabledToggle =
        noAction "toggling the trackpad"
    , printScreen =
        noAction "taking screenshots"
    , clipboardManager =
        def
    , lockscreen =
        noAction "locking the screen"
    , resetScreens =
        noAction "reseting the screens"
    , restartXMonad =
        noAction "restarting XMonad"
    , musicPlayPause =
        noAction "playing music"
    , musicNext =
        noAction "skipping music"
    , musicPrev =
        noAction "reverse skipping music"
    , defaultKeyboardLayout =
        noAction "switching to default keyboard layout"
    }

data Mode
  = Normal
  | Action
  | Workspaces
  deriving Show

use :: Mode -> X ()
use Normal = useMode normalMode
use mode = useMode $ show mode

-- Makes the below definitions prettier
(~>) :: String -> X () -> [(String, X ())]
k ~> a = [(k,a)]
infixr 0 ~>

(>>) :: Monoid m => m -> m -> m
(>>) = mappend

modes :: Actions -> XConfig Layout -> [(Name, Id, KeyMap)]
modes actions conf = let
    exit = "<Esc>" ~> use Normal
    mode :: Mode -> [(String, X ())] -> [(Name, Id, KeyMap)]
    mode n bs = [(show n, show n, mkKeymap conf $ exit ++ bs)]
  in do
    mode Action $ do
      "m" ~> use Workspaces
      -- Launchers
      "M5-<Space>" ~> menu actions
      "<Space>" ~> terminalPlain actions
      -- Window mgmt
      "d" ~> kill
      "j" ~> windows StackSet.focusDown
      "k" ~> windows StackSet.focusUp
      "S-j" ~> windows StackSet.swapDown
      "S-k" ~> windows StackSet.swapUp
      "S-<Space>" ~> sinkAll
      "f" ~> sendMessage $ Toggle FULL
      -- For BSP Layout
      "M5-k" ~> sendMessage $ ExpandTowards U
      "M5-j" ~> sendMessage $ ExpandTowards D
      "M5-h" ~> sendMessage $ ExpandTowards L
      "M5-l" ~> sendMessage $ ExpandTowards R
      "h" ~> sendMessage $ Rotate
      "l" ~> sendMessage $ Swap
      -- Misc
      "p" ~> trackpadEnabledToggle actions
      "c" ~> CM.toggleShow $ clipboardManager actions
      "M1-c" ~> CM.next $ clipboardManager actions
      "M1-S-c" ~> CM.prev $ clipboardManager actions
      "q" ~> lockscreen actions
      "M1-S-o" ~> resetScreens actions
      "M1-S-<Esc>" ~> liftIO$exitWith ExitSuccess

    mode Workspaces $ do
      "m" ~> use Action
      -- Cycling
      "M5-k" ~> prevWS
      "M5-j" ~> nextWS
      "M5-f" ~> cycleRecentWS [Keys.xK_Super_L] Keys.xK_Tab Keys.xK_grave
      -- Prompted workspace shifting
      "<Space>" ~>
        selectWorkspace actions >>= windows . StackSet.greedyView . init
      "S-<Space>" ~>
        selectWorkspace actions >>= windows . StackSet.shift . init
      -- Numeric workspace shifting
      let
        withBase prefix = zip (map (prefix ++) $ words "a s d f j k l")
        mapOp op = map (windows . op) (workspaces conf)
        screenOp op = map
          ((>>= flip whenJust (windows . op)) . screenWorkspace)
          [0..2]
      do
        withBase "" $ mapOp StackSet.greedyView
        withBase "S-" $ mapOp StackSet.shift
        withBase "M5-" $ screenOp StackSet.view
        withBase "M5-S-" $ screenOp StackSet.shift


normal :: Actions -> XConfig Layout -> KeyMap
normal actions conf = mkKeymap conf $ do
  "M-<Space>" ~> menu actions
  "M-S-<Space>" ~> terminalPlain actions

  "M5-S-<Esc>" ~> use Action
  "M5-S-<Return>" ~> use Workspaces

  -- Media keys
  "<XF86AudioRaiseVolume>" ~> volumeInc actions
  "<XF86AudioLowerVolume>" ~> volumeDec actions
  "<XF86AudioMute>" ~> volumeToggleMute actions
  "M1-<XF86AudioRaiseVolume>" ~> multiroomInc actions
  "M1-<XF86AudioLowerVolume>" ~> multiroomDec actions
  "M1-<XF86AudioMute>" ~> multiroomToggleMute actions
  "<XF86AudioPlay>" ~> musicPlayPause actions
  "<XF86AudioNext>" ~> musicNext actions
  "<XF86AudioPrev>" ~> musicPrev actions
  "<Print>" ~> printScreen actions

  -- misc
  -- "M-S-z" ~> spawn "xscreensaver-command -lock"
  "M-a" ~> defaultKeyboardLayout actions
  "M-<Esc>" ~> restartXMonad actions
