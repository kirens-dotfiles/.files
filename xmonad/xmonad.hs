---------------------------------------------------------------------------
--      _   _                                                            --
--      \\ //              Erik Nygren <dev@erik.work>                   --
--       \v/               https://github.com/Kirens                     --
--       /Λ\                                                             --
--      // \\ Monad        Current as of XMonad 0.13                     --
--       >>=                                                             --
---------------------------------------------------------------------------

------------------------------------------------------------------------}}}
-- Modules                                                              {{{
---------------------------------------------------------------------------
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86
--import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Util.Run

-- Date (for screenshots)
import Data.Time

-- Modifiy keybindings and use EMACS spec style bindings
import XMonad.Util.EZConfig

-- Arrow composition
import Control.Arrow hiding ((<+>), (|||))

-- Workspace cycling
import XMonad.Actions.CycleWS

-- Multiple monitors
import qualified XMonad.StackSet as W

-- Key-binding map
import qualified Data.Map as Map

-- Trancparent Inactive Windows
import XMonad.Hooks.FadeInactive


-----------------------------------------------------------------------}}}
-- Constants                                                           {{{
--------------------------------------------------------------------------

homeDir = "/home/kiren/"


-- Colors
------------

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- sizes
gap    = 10
topbar = 10
border = 0
prompt = 20
status = 20

myNormalBorderColor  = "#000000"
myFocusedBorderColor = active

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02




-----------------------------------------------------------------------}}}
-- Helpers / System-integration                                        {{{
--------------------------------------------------------------------------

-- xbacklight
--------------------------------------------------------------------------

bValues :: [Int] -- list of brightness levels on a log-scale
bValues = let l n = n : l (floor.(max 2).exp.(+0.5).log.fromIntegral $ n)
          in takeWhile (<100) (l 1) ++ [100]

-- | Compute current backlight-step and switch n steps
backlight :: Int -> X ()
backlight n = liftIO $ (runProcessWithInput "xbacklight" [] "")
    >>= ( read
      >>> round
      >>> (\n -> length (takeWhile (<n) bValues))
      >>> (+n)
      >>> (min $ length bValues - 1)
      >>> (max 0)
      >>> (bValues !!)
      >>> show
      >>> (\n-> runProcessWithInput "xbacklight" ["-set", n] "")
        )
    >> return ()



-- amixer
--------------------------------------------------------------------------

backlightUp = backlight 1
backlightDn = backlight (-1)

volume :: Int -> X ()
volume n = liftIO $ (runProcessWithInput "amixer get Master | grep -oe \"[0-9]*%\"" [] "")
    >>= \s -> case reads s of
      [(v,_)] -> runProcessWithInput "amixer" ["set", "Master", show (v+n) ++ "%"] ""
      _       -> return "" 
    >> return ()
volumeUp = volume 10
volumeDn = volume (-10)

vol :: Int -> X ()
vol 0 = liftIO $ runProcessWithInput ("amixer set Master " ++ show 0 ++ "%") [] ""
        >> return ()
vol n = liftIO $ (runProcessWithInput "xbacklight" [] "")
    >>= ( read
      >>> round
      >>> (\n -> length (takeWhile (<n) bValues))
      >>> (+n)
      >>> (min $ length bValues - 1)
      >>> (max 0)
      >>> (bValues !!)
      >>> show
      >>> (\n-> runProcessWithInput "echo 'a' > /home/kiren/a" [] "")
        )
    >> return ()


-- Background
--------------------------------------------------------------------------
type BackgroundName = String

defaultBackground :: BackgroundName
defaultBackground = "default.jpg"

setBkgrnd :: BackgroundName -> X ()
setBkgrnd n = spawn $ "feh --bg-fill $HOME/backgrounds/" ++ n

-- Introspective
--------------------------------------------------------------------------
editXmonadConfig :: X ()
editXmonadConfig = spawn "xterm -e \"vi $DOTFILES/xmonad/xmonad.hs\""

editTODO :: X ()
editTODO = spawn "xterm -e \"vi $DOTFILES/TODO\""

-- Screen grab
--------------------------------------------------------------------------
printScreen :: X ()
printScreen = dateTimeStr >>= (\n->
  spawn $ "import -silent -window root \"$HOME/screenshots/screenshot-"++n++".png\""
  )

dateTimeStr :: (MonadIO m) => m String
dateTimeStr = liftIO getCurrentTime >>= return.composite
  where
    composite t = date t ++ 'T' : time t
    date = show.utctDay
    time = show.timeToTimeOfDay.utctDayTime


-- Trackpad ctl
--------------------------------------------------------------------------
toggleTrackpad :: X ()
toggleTrackpad = spawn "$HOME/.xmonad/lib/scripts/lockptr"


-- Some screen stuff
--------------------------------------------------------------------------
-- This is a crude hack, that may or may not kill or restart compton. TODO
restartCompton :: X ()
restartCompton = spawn "sh -c 'ps -e | grep compton | grep -oe \"[0-9]*\" | head -1 | xargs kill'"

resetScreens :: X ()
resetScreens = spawn "xrandr --output eDP1 --mode 1920x1080 --primary --auto --output HDMI1 --off --output DP1 --off"

-----------------------------------------------------------------------}}}
-- Layouts                                                             {{{
--------------------------------------------------------------------------
myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 2/3

    -- Percent of screen to increment by when resizing panes
    delta = 5/100


-----------------------------------------------------------------------}}}
-- Startup                                                             {{{
--------------------------------------------------------------------------
startup = do
    setBkgrnd defaultBackground


-----------------------------------------------------------------------}}}
-- Main                                                                {{{
--------------------------------------------------------------------------



configBase = defaultConfig
  { manageHook  = manageDocks <+> manageHook defaultConfig
  , layoutHook  = myLayout --avoidStruts  $  layoutHook defaultConfig
  , logHook     = fadeInactiveLogHook 0.8
  , startupHook = startup
  , borderWidth = 0
  , modMask     = mod4Mask  -- Rebind Mod to the Windows key
  , keys        = myKeys
  } 

modNone :: KeyMask
modNone = 0

main = do
    xmonad configBase -- `additionalKeysP`

type KeyBnd = String
-- This should be made a better generalization (KeyBnd strings error prone) TODO
keysWithPar :: (KeyBnd -> a -> (KeyBnd, X ())) -> [KeyBnd] -> [a] -> [(KeyBnd, X ())]
keysWithPar fn ks as = [ fn k a | (k, a) <- zip ks as ]

numsWith :: KeyBnd -> [KeyBnd]
numsWith k = map ((k++).show) [1..9]

myKeys conf = mkKeymap conf $
  -- Quick launches
  [ ("M-a", spawn "dmenu_run")
  , ("M-s", spawn "xterm")
  , ("M-e", editXmonadConfig)
  , ("M-t", editTODO)

  -- Window mgmt
  , ("M-d",          kill)
  , ("M-<Return>",   windows W.focusMaster)
  , ("M-j",          windows W.focusDown)
  , ("M-k",          windows W.focusUp)
  , ("M-S-j",        windows W.swapDown)
  , ("M-S-k",        windows W.swapUp)
  , ("M-S-<Return>", windows W.swapMaster)

  -- Workspaces
  , ("M-C-<Left>",  prevWS)
  , ("M-C-h",       prevWS)
  , ("M-C-<Right>", nextWS)
  , ("M-C-l",       nextWS)
  , ("M-<Space>",   sendMessage NextLayout)

  -- Media keys
  , ("<XF86MonBrightnessUp>", backlightUp)
  , ("<XF86MonBrightnessDown>", backlightDn)
  , ("<XF86AudioRaiseVolume>", vol 1)
  , ("<XF86AudioLowerVolume>", vol (-1))
  , ("M-p", toggleTrackpad)
  , ("<Print>", printScreen)

  -- misc
  , ("M-o", resetScreens)
  , ("M-S-o", restartCompton)
  --, ("M-S-z", spawn "xscreensaver-command -lock")
  ]
  -- Switch workspace
  ++ keysWithPar 
       (\bind i -> (bind, windows $ W.greedyView i))
       (numsWith "M-")
       (XMonad.workspaces conf)
  -- Move window to workspace
  ++ keysWithPar 
       (\bind i -> (bind, windows $ W.shift i))
       (numsWith "M-S-")
       (XMonad.workspaces conf)

  -- Switch monitor
  ++ keysWithPar
       (\bind i -> (bind, screenWorkspace i >>= flip whenJust (windows . (W.view))))
       (numsWith "M-M1-")
       [0..2]
  -- Move window to monitors
  ++ keysWithPar
       (\bind i -> (bind, screenWorkspace i >>= flip whenJust (windows . (W.shift))))
       (numsWith "M-M1-S-")
       [0..2]

  -- `Start` is enough to open dmenu
  --`additionalKeys` [ ((modNone, xK_Super_L), (spawn "dmenu_run")) ]

{-
handleEventHook2   = handleEventHook defaultConfig `mappend`
                     keyUpEventHook `mappend`
                     fullscreenEventHook
-}
