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

-- Special keybindings
import XMonad.Util.EZConfig

-- Arrow composition
import Control.Arrow hiding ((<+>))

-- Workspace cycling
import XMonad.Actions.CycleWS

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
  , layoutHook  = avoidStruts  $  layoutHook defaultConfig
  , logHook     = fadeInactiveLogHook 0.8
  , startupHook = startup
  , borderWidth = 0
  , modMask     = mod4Mask  -- Rebind Mod to the Windows key
  } 

main = do
    xmonad $ configBase `additionalKeysP`
        [ ("M-<Space>", spawn "dmenu_run")
--        , ("M-<Enter>", spawn "xterm")
        , ("M-C-<Left>", prevWS)
        , ("M-C-<Right>", nextWS)
        , ("<XF86MonBrightnessUp>", backlightUp)
        , ("<XF86MonBrightnessDown>", backlightDn)
        , ("<XF86AudioRaiseVolume>", vol 1)
        , ("<XF86AudioLowerVolume>", vol (-1))
        --, ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        --, ((0, xK_Print), spawn "scrot")
        ]

{-
handleEventHook2   = handleEventHook defaultConfig `mappend`
                     keyUpEventHook `mappend`
                     fullscreenEventHook
-}
