---------------------------------------------------------------------------
--      _   _                                                            --
--      \\ //              Erik Nygren <dev@erik.work>                   --
--       \v/               https://github.com/Kirens                     --
--       /Λ\                                                             --
--      // \\ Monad        Current as of XMonad 0.13                     --
--       >>=                                                             --
---------------------------------------------------------------------------
-- Modules                                                              {{{
---------------------------------------------------------------------------
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Util.Run

-- My Config files
import XMonad.Config.Kirens.Keys as MyKeys

import qualified Prgms.ClipboardManager as CM


-- Gnome support stuff (e.g. getactivewindow)
import XMonad.Hooks.EwmhDesktops

-- swingstuff
import XMonad.Hooks.SetWMName

-- For default XMonad config
import XMonad.Config

-- To exit XMonad
import System.Exit

-- Layout Toggling
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- More Layouts
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition

-- Date (for screenshots)
import Data.Time

-- Arrow composition
import Control.Arrow hiding ((<+>), (|||))

-- Workspace cycling
import XMonad.Actions.CycleWS

-- Xmobar generating
import qualified Xmobar.MyConfig as MyXmobar

-- Nix store binaries
import qualified Nix.Vars as Pkgs

-- Key-binding map
import qualified Data.Map as Map

-- Transparent Inactive Windows
import XMonad.Hooks.FadeInactive

-- Detecting screens
import XMonad.Layout.IndependentScreens

-- Force windows to not float
import XMonad.Actions.SinkAll

-----------------------------------------------------------------------}}}
-- Constants                                                           {{{
--------------------------------------------------------------------------

homeDir = "/home/kiren/"
scriptsDir = homeDir++".xmonad/scripts/"
terminalBin = Pkgs.st
terminalCmd = terminalBin ++ " -f 'Hack:size=12'"

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

-- Monad tools
--------------------------------------------------------------------------

void :: MonadIO m => m a -> m ()
void = (>> return ())


-- Launching scripts
--------------------------------------------------------------------------

spawnScript = spawn.(scriptsDir++)

cmd cmd pars = liftIO $ runProcessWithInput cmd pars ""


-- Terminal stuff
--------------------------------------------------------------------------

terminalApp = terminalExec "tmux"
-- TODO: escape single quotes
terminalExec cmd = spawn$ terminalCmd ++ " -e \"" ++ cmd ++ "\""

vim = terminalExec.("vi\" \"" ++)


-- lock
--------------------------------------------------------------------------

lock = spawn Pkgs.i3Lock -- "xautolock -locknow"
autolockInit = spawn $ Pkgs.xautolock ++ " -time 15 -locker " ++ Pkgs.i3Lock


-- CopyQ
--------------------------------------------------------------------------

copyQ =
  def
    { CM.init = gen ""
    , CM.toggleShow = gen " -e 'if (visible()) hide(); else showAt(0, 0)'"
    , CM.next = gen " next"
    , CM.prev = gen " previous"
    }
  where
    gen c = spawn $ Pkgs.copyQ ++ c

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

backlightUp = backlight 1
backlightDn = backlight (-1)


-- amixer
--------------------------------------------------------------------------

setVol :: Int -> X ()
setVol n = spawn$ Pkgs.amixer ++ " set Master "++ show n

withVol :: (Num a, Read a) => (a -> X ()) -> X ()
withVol fn = void$ sequence.(return.fn.fst =<<).reads
  =<< cmd "sh" ["-c", Pkgs.amixer ++ " get Master | grep -oP '(?<=(?<!Limits: )Playback )[0-9]*' | head -1"]

linVol n  = withVol (setVol.(+n))

volumeUp = linVol 1
volumeDn = linVol (-1)

toggleMute = spawn$ Pkgs.amixer ++ " set Master 1+ toggle"

-- Menu
--------------------------------------------------------------------------

--menu = spawn$ Pkgs.rofi ++ " -matching fuzzy -modi combi -show combi -combi-modi drun"
menu = spawn$ Pkgs.rofi ++ " -terminal " ++ terminalBin ++ " -modi \"drun,scripts:$HOME/.config/rofi/scripts.select,window,ssh,calc:" ++ Pkgs.qalc ++ " +u8 -nocurrencies\" -show drun"

-- Background
--------------------------------------------------------------------------
type BackgroundName = String

defaultBackground :: BackgroundName
defaultBackground = "default.jpg"

setBkgrnd :: BackgroundName -> X ()
setBkgrnd n = spawn $ "feh --bg-fill $HOME/backgrounds/" ++ n

-- Introspective
--------------------------------------------------------------------------
--TODO: Temporarily hard-coded .files location
editXmonadConfig :: X ()
editXmonadConfig = vim $ Pkgs.dotfilesLocation ++ "/userspace/xmonad"

editTODO :: X ()
editTODO = vim $ Pkgs.dotfilesLocation ++ "/TODO"

reloadXMonad :: X ()
reloadXMonad = restart Pkgs.xmonad True
-- spawn $ Pkgs.xmonad ++ " --restart;"

-- Screen grab
--------------------------------------------------------------------------
printScreen :: X ()
printScreen = dateTimeStr >>=
  spawn.("import -silent -window root \"$HOME/screenshots/screenshot-"++).(++".png\"")

dateTimeStr :: (MonadIO m) => m String
dateTimeStr = liftIO getCurrentTime >>= return.composite
  where
    composite t = date t ++ 'T' : time t
    date = show.utctDay
    time = show.timeToTimeOfDay.utctDayTime


-- Trackpad ctl
--------------------------------------------------------------------------
toggleTrackpad :: X ()
toggleTrackpad = spawnScript "lockptr"


-- Some screen stuff
--------------------------------------------------------------------------
-- This is a crude hack, that may or may not kill or restart compton. TODO
restartCompton :: X ()
restartCompton = spawn "sh -c 'ps -e | grep compton | grep -oe \"[0-9]*\" | head -1 | xargs kill'"

resetScreens :: X ()
resetScreens = spawn "xrandr --output eDP1 --mode 1920x1080 --primary --auto --output HDMI1 --off --output DP1 --off --output HDMI2 --off"

-----------------------------------------------------------------------}}}
-- Status bar                                                          {{{
--------------------------------------------------------------------------

launchXmobar :: MonadIO m => FilePath -> m Handle
launchXmobar file = spawnPipe$ Pkgs.xmobar ++ " " ++ file


xmobarTitleColor = base3
xmobarCurrentWorkspaceColor = blue

xmobarConf :: Handle -> X ()
xmobarConf xmproc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn xmproc
  , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 100
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppSep    = "  "
  }

xmobarInactiveConf xmproc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn xmproc
  , ppTitle   = xmobarColor "green" "" . shorten 100
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppSep    = "  "
  }


-- Bar config
mkXmobarCfg :: Int -> IO FilePath
mkXmobarCfg s = do
-- name <- getName
  let name = "/tmp/xmobar"++show s
  writeFile name $ show $ MyXmobar.barOnScreen s
  return name

-----------------------------------------------------------------------}}}
-- Layouts                                                             {{{
--------------------------------------------------------------------------

myLayout = mkToggle (FULL ?? EOT)
         $ emptyBSP
         ||| spiral (6/7)
         ||| ThreeColMid 1 (3/100) (1/2)

myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  ]


-----------------------------------------------------------------------}}}
-- Startup                                                             {{{
--------------------------------------------------------------------------

startup = do
    setVol 0
    setBkgrnd defaultBackground
    autolockInit
    lock
    CM.init copyQ
    setWMName "LG3D"

-----------------------------------------------------------------------}}}
-- Main                                                                {{{
--------------------------------------------------------------------------

--main = xmonad.docks.configBase =<< launchXmobar
main = do
  -- Launch apropriately many xmobars
  nScreens <- countScreens
  xmobars <- sequence$ map ((>>=launchXmobar).mkXmobarCfg) [1..nScreens]
  xmonad$ewmh$docks$configBase xmobars

theLogHook xmproc =
  -- fade hook
  fadeInactiveLogHook 0.9
  >> xmprocing xmproc

-- xmprocing (p:r) = sequence (xmobarInactiveConf p :(map xmobarConf r))
-- xmprocing []    = return []

xmprocing = sequence.(zipWith (\n p -> if n==1 then xmobarInactiveConf p else xmobarConf p) [1..])


--configBase :: MonadIO Handle -> XConfig l
configBase xmproc =
  XConfig
  { XMonad.borderWidth =
      0
  , XMonad.workspaces =
      XMonad.workspaces def
  , XMonad.layoutHook =
      avoidStruts $ myLayout
  , XMonad.terminal =
      XMonad.terminal def
  , XMonad.normalBorderColor =
      XMonad.normalBorderColor def
  , XMonad.focusedBorderColor =
      XMonad.focusedBorderColor def
  , XMonad.modMask =
      mod4Mask
  , XMonad.keys =
      myKeys
  , XMonad.logHook =
      void (theLogHook xmproc)
  , XMonad.startupHook =
      startup
  , XMonad.mouseBindings =
      XMonad.mouseBindings def
  , XMonad.manageHook =
      manageDocks <+> manageHook def
  , XMonad.handleEventHook =
      XMonad.handleEventHook def
  , XMonad.focusFollowsMouse =
      XMonad.focusFollowsMouse def
  , XMonad.clickJustFocuses =
      XMonad.clickJustFocuses def
  , XMonad.clientMask =
      XMonad.clientMask def
  , XMonad.rootMask =
      XMonad.rootMask def
  , XMonad.handleExtraArgs =
      \ xs theConf ->
        case xs of
          [] -> return theConf
          _ -> fail ("unrecognized flags:" ++ show xs)
  }


-----------------------------------------------------------------------}}}
-- Key bindings                                                        {{{
--------------------------------------------------------------------------
myKeys =
  keyConfig$ def
  { MyKeys.menu =
      Main.menu
  , MyKeys.terminalPlain =
      terminalApp
  , MyKeys.editDotfiles =
      editXmonadConfig
  , MyKeys.editTODO =
      Main.editTODO
  , MyKeys.screenBrightnessInc =
      backlightUp
  , MyKeys.screenBrightnessDec =
      backlightDn
  , MyKeys.volumeInc =
      volumeUp
  , MyKeys.volumeDec =
      volumeDn
  , MyKeys.volumeToggleMute =
      toggleMute
  , MyKeys.trackpadEnabledToggle =
      toggleTrackpad
  , MyKeys.printScreen =
      Main.printScreen
  , MyKeys.clipboardManager =
      copyQ
  , MyKeys.lockscreen =
      lock
  , MyKeys.resetScreens =
      Main.resetScreens
  , MyKeys.restartXMonad =
      reloadXMonad
  }
