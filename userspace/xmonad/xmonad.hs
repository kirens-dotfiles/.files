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

import XMonad hiding (workspaces)
import qualified XMonad
import XMonad.Hooks.DynamicLog as DynLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Environment (getExecutablePath)
import System.Directory (getHomeDirectory)
import XMonad.Util.Run
import Control.Concurrent (forkIO)

-- Layout Info
import qualified XMonad.StackSet as S
import XMonad.Config.Prime as Prime (ScreenId ( S ))
import qualified XMonad.Util.NamedWindows as NamedW
import Data.Maybe (isJust, mapMaybe)
import Data.List (isInfixOf)
import Data.Bool (bool)

-- Format so XMobar understand
import Codec.Binary.UTF8.String (encodeString)

-- My Config files
import XMonad.Config.Kirens.Keys as MyKeys

import qualified Prgms.ClipboardManager as CM

-- Volume stuff
import XMonad.Actions.Volume (getVolume, setVolume)

-- Gnome support stuff (e.g. getactivewindow)
import XMonad.Hooks.EwmhDesktops

-- For default XMonad config
import XMonad.Config

-- To exit XMonad
import System.Exit

-- Layout Toggling
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- More Layouts
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

terminalBin = Pkgs.st
terminalCmd = terminalBin

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


specialWss = [ "spotify", "mail", "clipboard", "messaging" ]
workspaces = map show [1..9] ++ specialWss


-----------------------------------------------------------------------}}}
-- Helpers / System-integration                                        {{{
--------------------------------------------------------------------------

musicPlaying :: MonadIO m => m [ String ]
musicPlaying = do
  status <- cmd Pkgs.playerctl [ "-a", "status" ]
  players <- cmd Pkgs.playerctl [ "-l" ]
  let a =
       map fst
       $ filter ((== "Playing") . snd)
       $ zip (lines players) (lines status)
  exe Pkgs.playerctl [ show a ]
  exe Pkgs.playerctl [ show $ lines players ]
  exe Pkgs.playerctl [ show $ lines status ]
  exe Pkgs.playerctl [ show $ zip (lines players) (lines status) ]
  return a

musicPlaying' :: MonadIO m => m [ String ]
musicPlaying' = do
  s <- musicPlaying
  -- exe Pkgs.playerctl [ show s ]
  return s

playerctlWith :: MonadIO m => [ String ] -> String -> m String
playerctlWith args player =
  cmd Pkgs.playerctl ([ "--player=" ++ player ] ++ args)

playerctlWithPlaying :: MonadIO m => [ String ] -> m ()
playerctlWithPlaying args =
  musicPlaying
  >>= void . mapM (playerctlWith args)

play = exe Pkgs.playerctl [ "play" ]
pause = exe Pkgs.playerctl [ "-a", "pause" ]

playPause = do
  musicPlaying
  >>= bool pause play . null

prev = playerctlWithPlaying [ "previous" ]
next = playerctlWithPlaying [ "next" ]

-- Spotify Ctl

spotifySend cmd =
  spawn
    ( Pkgs.dbusSend
    ++ " --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player"
    ++ cmd
    )
  >> return ()

-- Monad tools
--------------------------------------------------------------------------

void :: MonadIO m => m a -> m ()
void = (>> return ())


-- Launching scripts
--------------------------------------------------------------------------

run bin args inp = liftIO $ runProcessWithInput bin args inp
cmd bin args = run bin args ""
exe bin args = void $ cmd bin args

-- Terminal stuff
--------------------------------------------------------------------------

terminalApp = terminalExec Pkgs.tmux
-- TODO: escape single quotes
terminalExec cmd = spawn$ terminalCmd ++ " -e \"" ++ cmd ++ "\""

-- lock
--------------------------------------------------------------------------

lock = exe Pkgs.xautolock [ "-locknow" ]

-- KeyboardLayout
--------------------------------------------------------------------------

pickDefaultLayout = exe Pkgs.xkbcomp [Pkgs.customKeymap, ":0"]

-- CopyQ
--------------------------------------------------------------------------

copyQ =
  def
    { CM.init = gen ""
    , CM.toggleShow = gen " -e 'if (visible()) hide(); else show()'"
    , CM.next = gen " next"
    , CM.prev = gen " previous"
    }
  where
    gen c = spawn $ Pkgs.copyQ ++ c

-- amixer
--------------------------------------------------------------------------

linVol n = getVolume >>= setVolume . (+n)

-- Apparently increasing 1 is to small to propagate
volumeUp = linVol 2
volumeDn = linVol (-3)

toggleMute = spawn$ Pkgs.amixer ++ " set Master 1+ toggle"

-- Menu
--------------------------------------------------------------------------

--menu = spawn$ Pkgs.rofi ++ " -matching fuzzy -modi combi -show combi -combi-modi drun"
menu = spawn $ unwords
  [ Pkgs.rofi
  , "-terminal " ++  terminalBin
  , "-modi \"combi,window,ssh,calc:" ++ Pkgs.qalc ++ " +u8 -nocurrencies\""
  , "-show combi"
  , "-combi-modi \"drun,scripts:" ++ Pkgs.rofiScripts ++ "\""
  ]

rofi lines name input =
  run
    Pkgs.rofi
    [ "-dmenu"
    , "-lines"
    , show lines
    , "-p"
    , name
    ]
    (unlines input)

-- Background
--------------------------------------------------------------------------

setRandomBg :: X ()
setRandomBg = liftIO $ void $ forkIO $ io $ exe Pkgs.randomBgScript []

-- Screen grab
--------------------------------------------------------------------------
printScreen :: X ()
printScreen = do
  date <- dateTimeStr
  home <- liftIO getHomeDirectory
  exe Pkgs.screenshot
    [ "-silent"
    , "-window"
    , "root"
    , home ++ "/screenshots/screenshot-" ++ date ++ ".png"
    ]

dateTimeStr :: (MonadIO m) => m String
dateTimeStr = liftIO getCurrentTime >>= return.composite
  where
    composite t = date t ++ 'T' : time t
    date = show.utctDay
    time = show.timeToTimeOfDay.utctDayTime


-- Trackpad ctl
--------------------------------------------------------------------------
toggleTrackpad :: X ()
toggleTrackpad = void $ cmd Pkgs.lockptrScript []


-- Some screen stuff
--------------------------------------------------------------------------
-- This is a crude hack, that may or may not kill or restart compton. TODO
restartCompton :: X ()
restartCompton = spawn "sh -c 'ps -e | grep compton | grep -oe \"[0-9]*\" | head -1 | xargs kill'"

resetScreens :: X ()
resetScreens = spawn "xrandr --output eDP1 --mode 1920x1080 --primary --auto --output HDMI1 --off --output DP1 --off --output HDMI2 --off"


-- Some screen stuff
--------------------------------------------------------------------------

restart = io getExecutablePath >>= flip XMonad.restart True


-----------------------------------------------------------------------}}}
-- Status bar                                                          {{{
--------------------------------------------------------------------------

launchXmobar :: MonadIO m => FilePath -> m Handle
launchXmobar file = spawnPipe$ Pkgs.xmobar ++ " " ++ file


xmobarTitleColor = base3
xmobarCurrentWorkspaceColor = blue


-- Bar config
mkXmobarCfg :: Int -> IO FilePath
mkXmobarCfg s = do
-- name <- getName
  let name = "/tmp/xmobar"++show s
  writeFile name $ show $ MyXmobar.barOnScreen s
  return name

xmobarHook :: ScreenId -> Handle -> X ()
xmobarHook screenId proc =
  xmobarPrint screenId proc >>= io . hPutStrLn proc . encodeString

xmobarPrint :: ScreenId -> Handle -> X String
xmobarPrint screenId proc = do
  winset <- gets windowset

  let
    activeWs = S.currentTag winset
    visibleWss =
      map (S.tag . S.workspace) $ S.visible winset
    hiddenWss =
      filter (not . flip elem specialWss)
      $ map S.tag
      $ filter (isJust . S.stack)
      $ S.hidden winset
    ws = unwords $ flip mapMaybe workspaces $ \ws ->
      if ws == activeWs
      then return $ xmobarClr green ws
      else if ws `elem` visibleWss
      then return $ "("++ ws ++")"
      else if ws `elem` hiddenWss
      then return ws
      else Nothing

  wt <- maybe (return "") (fmap show . NamedW.getName) . S.peek $ winset
  let title = DynLog.xmobarStrip wt

  let activeScreen = S.screen $ S.current winset

  if screenId == activeScreen
  then return $ ws ++ " " ++ wt
  else return $ xmobarClr base0 $ ws ++ " " ++ wt

xmobarClr clr text =
  "<fc=" ++ clr ++ ">" ++ text ++ "</fc>"

-----------------------------------------------------------------------}}}
-- Layouts                                                             {{{
--------------------------------------------------------------------------

myLayout = mkToggle (FULL ?? EOT) emptyBSP

-----------------------------------------------------------------------}}}
-- Startup                                                             {{{
--------------------------------------------------------------------------

startup = do
    setVolume 0
    setRandomBg
    CM.init copyQ

-----------------------------------------------------------------------}}}
-- Main                                                                {{{
--------------------------------------------------------------------------

--main = xmonad.docks.configBase =<< launchXmobar
main = do
  -- Launch apropriately many xmobars
  nScreens <- countScreens
  xmobars <- sequence$ map ((>>=launchXmobar).mkXmobarCfg) [0..nScreens-1]
  XMonad.xmonad $ ewmh $ docks $ configBase xmobars

theLogHook xmproc =
  -- fade hook
  fadeInactiveLogHook 0.9
  >> sequence (zipWith xmobarHook [Prime.S 0..] xmproc)

--configBase :: MonadIO Handle -> XConfig l
configBase xmproc =
  XConfig
  { XMonad.borderWidth =
      0
  , XMonad.workspaces =
      workspaces
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
  , XMonad.manageHook = composeAll
   [ className =? "Spotify" --> doShift "spotify"
   , className =? "Thunderbird" --> doShift "mail"
   , className =? "CopyQ" --> doShift "clipboard"
   , className =? "Franz" --> doShift "messaging"
   , className =? "Signal" --> doShift "messaging"
   , className =? "Xmessage"  --> doFloat
   , manageDocks
   ]
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
  , MyKeys.selectWorkspace =
      rofi 10 "Select Workspace" workspaces
  , MyKeys.terminalPlain =
      terminalApp
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
      Main.restart
  , MyKeys.musicPlayPause =
      playPause
  , MyKeys.musicNext =
      next
  , MyKeys.musicPrev =
      prev
  , MyKeys.defaultKeyboardLayout =
      pickDefaultLayout
  }
