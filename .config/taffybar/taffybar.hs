-- -*- mode:haskell -*-
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Char
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Graphics.UI.Gtk as G
import Control.Monad.Trans (liftIO)
import System.Exit (ExitCode)
import System.IO (hPutStr, hClose)
import System.Process
import Data.IORef
import Text.Printf
import System.Taffybar
import qualified System.Taffybar.Context as C
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Util (logPrintF)
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces


transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = transparent
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just $ T.pack "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just $ T.pack "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just $ T.pack "cpu"
  }

-- TC: KTVC
-- Dowagiac: KBEH
wcfg = (defaultWeatherConfig "KTVC") { weatherTemplate = "$stationPlace$ : $tempF$ F / $tempC$ C - $skyCondition$" }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]


{-shellWidgetNew defaultStr cmd interval = do-}
  {-{-label <- pollingLabelNew (T.pack defaultStr) interval $ return T.pack `ap` ( stripStr $ readCreateProcess (shell cmd) "")-}-}
  {-label <- pollingLabelNew defaultStr interval $ T.pack (liftIO $ stripStr $ readCreateProcess (shell cmd) "")-}
  {-liftIO $ G.widgetShowAll $ label-}
  {-return label-}

stripStr :: IO String -> IO String
stripStr ioString = do
  str <- ioString
  return $ rstrip $ str

rstrip = reverse . dropWhile isSpace . reverse


main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      clock = textClockNew Nothing "%a %b %_d %r" 1
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      {-updates = shellWidgetNew "..." "echo -e \"Updates: $(eix -u# | wc -l)\"" 5-}
      {-kernel = shellWidgetNew "..." "echo -e \"Cur: $(uname -r)\"" 86400 -}
      {-newKernel = shellWidgetNew "..." "echo -e \"New: $(newkern)\"" 1800-}
      weather = liftIO $ weatherNew wcfg 10
          -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
          -- for a better way to set up the sni tray
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt


      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [ layout, windows ]
        , endWidgets = map (>>= buildContentsBox)
          [ batteryIconNew
          , clock
          , tray
          , cpu
          , mem
          , net
          , mpris2New
	  {-, updates-}
          {-, kernel-}
          {-, newKernel-}
          , weather
          ]
        , barPosition = Top
        , barPadding = 10
        , barHeight = 50
        , widgetSpacing = 8
        }
  startTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
               toTaffyConfig myConfig
