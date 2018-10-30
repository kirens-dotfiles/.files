-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Erik Nygren
--                Derivative of Xmobar.Config (c) Andrea Rossato from
--                https://github.com/jaor/xmobar/
-- License     :  BSD-style (see LICENSE in current directory)
--
-- The configuration Constructor of Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Config where

import Xmobar.Interfaces
import Data.Function

data Config = Config
  { font :: String
  , additionalFonts :: [String]
  , bgColor :: String
  , fgColor :: String
  , position :: XPosition
  , textOffset :: Int
  , iconOffset :: Int
  , border :: Border
  , borderColor :: String
  , borderWidth :: Int
  , alpha :: Int
  , hideOnStart :: Bool
  , allDesktops :: Bool
  , overrideRedirect :: Bool
  , pickBroadest :: Bool
  , lowerOnStart :: Bool
  , persistent :: Bool
  , iconRoot :: FilePath
  , commands :: [Runnable]
  , sepChar :: String
  , alignSep :: String
  , template :: String
  } deriving (Show)

data XPosition = Top
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
    deriving ( Show, Eq )

data Align = L | R | C deriving ( Show, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
    deriving ( Show, Eq )

defaultXmobar :: Config
defaultXmobar = Config
  { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
  , additionalFonts = []
  , bgColor = "#000000"
  , fgColor = "#BFBFBF"
  , alpha   = 255
  , position = Top
  , border = NoBorder
  , borderColor = "#BFBFBF"
  , borderWidth = 1
  , textOffset = -1
  , iconOffset = -1
  , hideOnStart = False
  , lowerOnStart = True
  , persistent = False
  , allDesktops = True
  , overrideRedirect = True
  , pickBroadest = False
  , iconRoot = "."
  , commands = [ Run $ Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
               , Run StdinReader]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ " ++
               "<fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
  }
