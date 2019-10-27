module XMonad.Actions.ModalKeyActions (
  KeyMap, Id, Name
  , prepareMode, prepareNamedMode
  , useMode, getKeys, getMode, normalMode
  , eventHook
) where

{-# LANGUAGE DeriveDataTypeable #-}
import XMonad ( Display
              , Event ( KeyEvent )
              , ExtensionClass ( initialValue )
              , KeyCode
              , KeyMask
              , KeySym
              , Typeable
              , Window
              , X
              , XConf ( XConf )
              , anyKey
              , anyModifier
              , ask
              , asks
              , cleanMask
              , config
              , display
              , displayKeycodes
              , ev_event_type
              , ev_keycode
              , ev_state
              , extraModifiers
              , grabKey
              , grabModeAsync
              , io
              , keyActions
              , keyPress
              , keycodeToKeysym
              , logHook
              , theRoot
              , ungrabKey
              , userCodeDef
              , whenJust
              , withDisplay
              , (.|.)
              )
import Prelude ( Bool (True, False)
               , IO
               , Maybe (Nothing, Just)
               , String
               , fmap
               , fromIntegral
               , fst
               , id
               , return
               , snd
               , ($)
               , (++)
               , (.)
               , (=<<)
               , (==)
               , (>>=)
               )
import Data.Composition ( (.:.) )
import Control.Monad ( forM, forM_, mapM_, join, when )
import Data.Monoid ( All ( All ) )
import qualified XMonad.Util.ExtensibleState as XS
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified XMonad.Util.PureX as PX

-- Basic data type
type Id = String
type Name = String
normalMode = "normal"

type KeyMap = Map (KeyMask, KeySym) (X ())

type Mode = (Name, KeyMap)

data ActionModes = ActionModes Mode (Map Id Mode)
  deriving (Typeable)

instance ExtensionClass ActionModes where
  initialValue = ActionModes m (Map.singleton normalMode m)
    where m = (normalMode, Map.empty)

insert :: Name -> Id -> KeyMap -> ActionModes -> ActionModes
insert n i newM (ActionModes m ms) =
  ActionModes m (Map.insert i (n, newM) ms)

use :: Id -> ActionModes -> ActionModes
use newId a@(ActionModes _ ms) =
  case Map.lookup newId ms of
    Nothing   -> a
    Just newM -> ActionModes newM ms

useMap :: Name -> KeyMap -> ActionModes -> ActionModes
useMap n km (ActionModes _ ms) = ActionModes (n, km) ms

get :: ActionModes -> Mode
get (ActionModes m _) = m

name :: ActionModes -> Name
name = fst . get

keys :: ActionModes -> KeyMap
keys = snd . get

-- X wraped stuff
prepareNamedMode :: PX.XLike m => Name -> Id -> KeyMap -> m ()
prepareNamedMode = XS.modify .:. insert

prepareMode :: PX.XLike m => Id -> KeyMap -> m ()
prepareMode id = prepareNamedMode id id

getMode :: PX.XLike m => m Name
getMode = name `fmap` XS.get

getKeys :: PX.XLike m => m KeyMap
getKeys = keys `fmap` XS.get


-- | Switch what mode is being used and recalculate the keys grabbed by X.
-- Note that `"normal"` is a magic word that uses the keymap defined in the
-- config
useMode :: Id -> X ()
useMode id = do
  if id == normalMode
  then do
    normalKeys <- asks keyActions
    XS.modify $ useMap normalMode normalKeys
  else XS.modify $ use id

  -- Run logHook since changing mode
  join $ asks $ logHook . config

  ks <- getKeys
  grabKeys ks


-- | An event hook to overwrite the default KeyEvent handler
eventHook :: Event -> X All
eventHook (KeyEvent
            { ev_event_type = t
            , ev_state = m
            , ev_keycode = code
            }) | t == keyPress =
  withDisplay $ \dpy -> do
    s  <- io $ keycodeToKeysym dpy code 0
    mClean <- cleanMask m
    ks <- getKeys
    userCodeDef () $ whenJust (Map.lookup (mClean, s) ks) id
    -- If we happen to get into a mode without bindings we'd like to escape
    when (Map.keys ks == []) $ useMode normalMode
    return $ All False
eventHook _ = return $ All True


-- | Tells X to grab the keys specified in the mode
grabKeys :: KeyMap -> X ()
grabKeys ks = do
  XConf { display = dpy, theRoot = rootw } <- ask
  io $ ungrabKey dpy anyKey anyModifier rootw

  keysymToKeycodes <- io $ getKeyboardMapping dpy

  let grab :: KeyCode -> KeyMask -> X ()
      grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync

  forM_ (Map.keys ks) $ \(mask,sym) ->
    forM_ (keysymToKeycodes sym) $ \kc ->
      mapM_ (grab kc . (mask .|.)) =<< extraModifiers


-- | Construct a function that can map KeySyms to KeyCodes
getKeyboardMapping :: Display -> IO (KeySym -> [KeyCode])
getKeyboardMapping dpy = let
    (minCode, maxCode) = displayKeycodes dpy
    allCodes = [fromIntegral minCode .. fromIntegral maxCode]
    withKeysym code = (\sym -> (sym, [code])) `fmap` keycodeToKeysym dpy code 0
  in do
    symCodes <- forM allCodes withKeysym
    let lookupTable = Map.fromListWith (++) symCodes

    return $ \sym ->
      Map.findWithDefault [] sym lookupTable
