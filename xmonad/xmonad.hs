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

-- Date (for screenshots)
import Data.Time

-- Modifiy keybindings and use EMACS spec style bindings
import XMonad.Util.EZConfig

-- Arrow composition
import Control.Arrow hiding ((<+>), (|||))

-- Workspace cycling
import XMonad.Actions.CycleWS

-- Xmobar generating
import qualified Xmobar.MyConfig as MyXmobar

-- Multiple monitors
import qualified XMonad.StackSet as W

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
terminalName = "termite"

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

terminalApp = spawn terminalName
-- TODO: escape single quotes
terminalExec cmd = spawn (terminalName ++ " -e \"fish -c '" ++ cmd ++ "' \"")

-- lock
--------------------------------------------------------------------------

lock = spawn "i3lock-fancy" -- "xautolock -locknow"


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
setVol n = spawn$ "amixer set Master "++ show n

withVol :: (Num a, Read a) => (a -> X ()) -> X ()
withVol fn = void$ sequence.(return.fn.fst =<<).reads
  =<< cmd "sh" ["-c", "amixer get Master | grep -oP '(?<=(?<!Limits: )Playback )[0-9]*' | head -1"]

linVol n  = withVol (setVol.(+n))

volumeUp = linVol 1000
volumeDn = linVol (-1000)

toggleMute = spawn "amixer set Master 1+ toggle"


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
editXmonadConfig = terminalExec "vi $DOTFILES/xmonad/"

editTODO :: X ()
editTODO = terminalExec "vi $DOTFILES/TODO"

reloadXMonad :: X ()
reloadXMonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

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
launchXmobar file = spawnPipe$ "xmobar " ++ file


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
         $ tiled ||| Mirror tiled ||| ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2) ||| spiral (6/7)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 2/3

    -- Percent of screen to increment by when resizing panes
    delta = 5/100


myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  ]


-----------------------------------------------------------------------}}}
-- Startup                                                             {{{
--------------------------------------------------------------------------

startup = do
    setVol 0
    setBkgrnd defaultBackground
    spawn "xautolock -time 15 -locker i3lock-fancy"
    lock

-----------------------------------------------------------------------}}}
-- Main                                                                {{{
--------------------------------------------------------------------------

--main = xmonad.docks.configBase =<< launchXmobar
main = do
  -- Launch apropriately many xmobars
  nScreens <- countScreens
  xmobars <- sequence$ map ((>>=launchXmobar).mkXmobarCfg) [1..nScreens]
  xmonad$docks$configBase xmobars

theLogHook xmproc =
  -- fade hook
  fadeInactiveLogHook 0.9
  >> xmprocing xmproc

-- xmprocing (p:r) = sequence (xmobarInactiveConf p :(map xmobarConf r))
-- xmprocing []    = return []

xmprocing = sequence.(zipWith (\n p -> if n==1 then xmobarInactiveConf p else xmobarConf p) [1..])


--configBase :: MonadIO Handle -> XConfig l
configBase xmproc = def
  { manageHook  = manageDocks <+> manageHook def
  , layoutHook  = avoidStruts $ myLayout
  -- void the log hook to have a X ()
  , logHook     = void (theLogHook xmproc)
  , startupHook = startup
  , borderWidth = 0
  , modMask     = mod4Mask  -- Rebind Mod to the Windows key
  , keys        = myKeys
  } 


-----------------------------------------------------------------------}}}
-- Key bindings                                                        {{{
--------------------------------------------------------------------------

type KeyBnd = String

modNone :: KeyMask
modNone = 0

-- This should be made a better generalization (KeyBnd strings error prone) TODO
keysWithPar :: (KeyBnd -> a -> (KeyBnd, X ())) -> [KeyBnd] -> [a] -> [(KeyBnd, X ())]
keysWithPar fn ks as = [ fn k a | (k, a) <- zip ks as ]

numsWith :: KeyBnd -> [KeyBnd]
numsWith k = map ((k++).show) [1..9]

myKeys conf = mkKeymap conf $
  -- Quick launches
  [ ("M-a", spawn "dmenu_run")
  , ("M-s", terminalApp)
  , ("M-e", editXmonadConfig)
  , ("M-t", editTODO)

  -- Window mgmt
  , ("M-d", kill)
  , ("M-<Return>", windows W.focusMaster)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-<Return>", windows W.swapMaster)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-<Space>", sinkAll)

  -- Workspaces
  , ("M-C-h", prevWS)
  , ("M-C-l", nextWS)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-f", sendMessage $ Toggle FULL)

  -- Media keys
  , ("<XF86MonBrightnessUp>", backlightUp)
  , ("<XF86MonBrightnessDown>", backlightDn)
  , ("<XF86AudioRaiseVolume>", volumeUp)
  , ("<XF86AudioLowerVolume>", volumeDn)
  , ("<XF86AudioMute>", toggleMute)
  , ("M-p", toggleTrackpad)
  , ("<Print>", printScreen)

  -- misc
  , ("M-q", lock)
  , ("M-M1-S-o", resetScreens)
  , ("M-M1-o", restartCompton)
  , ("M-<Esc>", reloadXMonad)
  , ("M-S-<Esc>", liftIO$exitWith ExitSuccess)
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
