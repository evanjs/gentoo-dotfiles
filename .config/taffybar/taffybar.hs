-- -*- mode:haskell -*-
module Main where
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Char (isSpace)
import qualified Graphics.UI.Gtk as G
import Control.Monad.Trans (liftIO)
import System.Taffybar.Compat.GtkLibs
{-import GI.Gtk.Objects.Widget-}
import System.Exit (ExitCode)
import System.IO (hPutStr, hClose)
import System.Process
import Data.IORef
import Text.Printf
{-import System.Process.Typed-}
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
  , graphLabel = Just "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just "cpu"
  }

wcfg = (defaultWeatherConfig "KIH22")
  { weatherTemplate = "$tempC$ C / $humidity$" }

{-updateCfg = (pollingConfig 10)-}
  {-{ }-}

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

{-getLines n = sequence $ replicate n getLine-}

{-shellWidgetNew :: String -> String -> Double -> C.TaffyIO G.Widget-}
{-shellWidgetNew :: String -> String -> [String] -> Double -> C.TaffyIO G.Widget-}
shellWidgetNew defaultStr cmd interval = do
  {-label <- pollingLabelNew defaultStr interval $ stripStr $ readProcess cmd [] []-}
  {-count <- readCreateProcess (shell cmd) ""-}
  label <- pollingLabelNew defaultStr interval $ stripStr $ readCreateProcess (shell cmd) "" 
  {-liftIO $ label-}
  {-liftIO $ G.widgetQueueResizeNoRedraw label-}
  {-let align = G.alignmentNew 0.0 0.5 0.0 0.0-}
  {-let widget = G.widget { label :: Label, align :: Alignment i}-}
  liftIO $ G.widgetShowAll $ label
  --liftIO $ G.widgetQueueResize label
  return label


{-updateWidgetNew :: C.TaffyIO G.Widget-}
{-updateWidgetNew = do-}
	{-lines <- readCreateProcess (shell "eix -u# | wc -l") ""-}
	{-{-packages <- readProcess "eix" ["-u", "-#"] []-}-}
	{-{-lines <- readProcess "wc" ["-l"] $ packages-}-}
	{-{-lines <- createProcess (proc "eix -u# | wc -l")-}-}
	{-{-(_, Just hout, _, _) <--}-}
		{-{-createProcess (proc "ls" []){ std_out = CreatePipe }-}-}
	{-{-liftIO $ lines-}-}
	{-{-(_, Just so, _, ph1)  <- createProcess (proc "eix" ["-u","-#"]) { std_out = CreatePipe } -}-}
	{-{-lines <- readProcess "eix" ["-u", "-#"] [] -}-}
	{-{-(_, Just pipeout, _, _) <- createProcess (proc "eix" ["-u", -"#"]){ std_out = CreatePipe }-}-}
	{-{-(_, _, _, ph)  <- createProcess (proc "wc" ["-l"]){ std_in = UseHandle pipeout }-}-}
	{-{-_ <- waitForProcess ph-}-}
	{-{-lines <- hPutStr pipeout	-}-}
	{-{-liftIO $ lines-}-}
        {-{-createProcess (proc "wc" ["-l"]) { std_in = UseHandle so }-}-}
	{-{-lines <- readProcess so-}-}
	{-label <- pollingLabelNew "..." 5 $ stripStr lines-}
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
      {-weather = weatherNew wcfg 10-}
      {-updates = shellWidgetNew "..." "eix" ["-u", "-#"] 5-}
      updates = shellWidgetNew "..." "eix -u# | wc -l" 5
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
          {-, weather-}
          , updates
          ]
        , barPosition = Top
        , barPadding = 10
        , barHeight = 50
        , widgetSpacing = 0
        }
  startTaffybar $ withBatteryRefresh $ withLogServer $ withToggleServer $
               toTaffyConfig myConfig
