{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Erik Nygren
--                Derivative of xmobar/src/Plugins/* from
--                https://github.com/jaor/xmobar/
--                (c) Jochen Keil - Plugins.BufferedPipeReader
--                (c) John Goerzen - Plugins.CommandReader
--                (c) Andrea Rossato - Plugins.Date,
--                                     Plugins.PipeReader,
--                                     Plugins.StdinReader,
--                                     Plugins.Monitors (2007-10)
--                (c) Martin Perner - Plugins.DateZone,
--                                    Plugins.Kbd
--                (c) Spencer Janssen - Plugins.EWMH,
--                                      Plugins.Mail,
--                                      Plugins.XMonadLog
--                (c) Patrick Chilton - Plugins.Locks
--                (c) Jose A Ortega Ruiz - Plugins.MBox,
--                                         Plugins.Utils,
--                                         Plugins.Monitors (2010-13, 17)
--                (c) Reto Habluetzel - Plugins.MarqueePipeReader
--
--
--
-- License     :  BSD-style (see LICENSE in current directory)
--
-- The configuration Constructor of Xmobar
--
-----------------------------------------------------------------------------


module Xmobar.Interfaces where

-- Type aliases
type Args      = [String]
type Program   = String
type Alias     = String
type Station   = String
type Zone      = String
type ZoneNo    = Int
type Interface = String
type Rate      = Int
type DiskSpec  = [(String, String)]
type Length = Int
type Separator = String



{- Runnable --------------------------------------------}
-- |
data Runnable = forall r . (Show r) => Run r

instance Show Runnable where
    show (Run r) = "Run " ++ show r

{- Command ---------------------------------------------}
-- |
data Command = Com Program Args Alias Rate 
    deriving (Show,Eq)

{- Date ------------------------------------------------}
-- |
data Date = Date String String Int
    deriving (Show)

{- CommandReader ---------------------------------------}
-- |
data CommandReader = CommandReader String String
    deriving (Show)

{- BufferPipeReader ------------------------------------}
-- |
data BufferedPipeReader = BufferedPipeReadr String [(Int, Bool, String)]
    deriving (Show)

{- XMonadLog -------------------------------------------}
-- |
data XMonadLog = XMonadLog
               | UnsafeXMonadLog
               | XPropertyLog String
               | UnsafeXPropertyLog String
               | NamedXPropertyLog String String
               | UnsafeNamedXPropertyLog String String
    deriving (Show)

{- EWMH ------------------------------------------------}
-- |
data EWMH = EWMH | EWMHFMT Component
    deriving (Show)

-- |
data Component = Text String
               | Component :+ Component
               | Modifier :$ Component
               | Sep Component [Component]
               | WindowName
               | Layout
               | Workspaces [WsOpt]
    deriving (Show)

-- |
data Modifier = Hide
              | Color String String
              | Short Int
              | Wrap String String
    deriving (Show)

-- |
data WsOpt = Modifier :% WsType
           | WSep Component
    deriving (Show)
infixr 0 :%

-- |
data WsType = Current | Empty | Visible
    deriving (Show, Eq)

{- Kbd -------------------------------------------------}
-- |
data Kbd = Kbd [(String, String)]
    deriving (Show)

{- Locks -----------------------------------------------}
-- |
data Locks = Locks
    deriving (Show)

-- | A list of mail box names and paths to maildirs.
data Mail = Mail [(String, FilePath)] String
    deriving (Show)

{- Pipe Reader -----------------------------------------}
-- |
data PipeReader = PipeReader String String
    deriving (Show)

{- StdinReader -----------------------------------------}
-- |
data StdinReader = StdinReader | UnsafeStdinReader
    deriving (Show)

{- MBox ------------------------------------------------}
-- |
data MBox = MBox [(String, FilePath, String)] [String] String
    deriving (Show)

{- DateZone --------------------------------------------}
-- |
data DateZone = DateZone String String String String Int
    deriving (Show)

{- MarqueePipeReader -----------------------------------}
-- |
data MarqueePipeReader = MarqueePipeReader String (Length, Rate, Separator) String
    deriving (Show)


{- Monitors --------------------------------------------}
-- |
data Monitors = Network      Interface    Args Rate
              | DynNetwork                Args Rate
              | BatteryP     Args         Args Rate
              | BatteryN     Args         Args Rate Alias
              | Battery      Args         Rate
              | DiskU        DiskSpec     Args Rate
              | DiskIO       DiskSpec     Args Rate
              | Thermal      Zone         Args Rate
              | ThermalZone  ZoneNo       Args Rate
              | Memory       Args         Rate
              | Swap         Args         Rate
              | Cpu          Args         Rate
              | MultiCpu     Args         Rate
              | Brightness   Args         Rate
              | CpuFreq      Args         Rate
              | CoreTemp     Args         Rate
              | TopProc      Args         Rate
              | TopMem       Args         Rate
              | Uptime       Args         Rate
              | CatInt       Int FilePath Args Rate
              | Weather      Station      Args Rate
              | UVMeter      Station      Args Rate
              | Wireless     Interface    Args Rate
              | MPD          Args         Rate
              | AutoMPD      Args
              | Volume       String       String Args Rate
              | Mpris1       String       Args Rate
              | Mpris2       String       Args Rate
    deriving (Show,Eq)
