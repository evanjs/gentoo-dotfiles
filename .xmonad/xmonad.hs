#! /usr/bin/env runhugs +l
--
-- xmonad.hs
-- Copyright (C) 2018 evanjs <evanjs@asentoo>
--
-- Distributed under terms of the MIT license.
--



import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.NoBorders
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicBars as Bars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

import qualified Data.Map         as M
import qualified XMonad.StackSet  as W

------------------------------------------------------------------------

myTerminal = "kitty"

-- The command to lock the screen or show the screensaver.
myLock = "slimlock"
myHalfLock = "xtrlock"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "maim -s ~/shots/$(date +%Y-%m-%d_%T).png"

-- The command to take a fullscreen screenshot.
myScreenshot = "maim > ~/shots/$(date +%Y-%m-%d_%T).png"

myDelayedScreenshot = "maim -d3 ~/shots/$(date +%Y-%m-%d_%T).png"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "rofi -lines 7 -columns 2 -modi run -show"
mySshLauncher = "rofi -lines 7 -columns 2 -modi ssh -show"

------------------------------------------------------------------------
-- workspaces

myWorkspaces = ["web", "2", "3", "4", "5", "6", "7", "8", "9"] ++ map show [11.999]

kill8 ss | Just w <- W.peek ss = W.insertUp w $ W.delete w ss
         | otherwise = ss

------------------------------------------------------------------------
-- layouts

tabbedLayout = tabbed shrinkText tabbedConf

tabbedConf = def
  {
    fontName = "xft:fira-code"
  }

genericLayouts =
    avoidStruts $
    smartBorders (tall ||| Mirror tall ||| tabbedLayout ||| noBorders (fullscreenFull Full))
        where tall = Tall 1 (3/100) (1/2)

myLayouts = genericLayouts


------------------------------------------------------------------------
-- window rules

--myManageHook = scratchpadManageHookDefault <+>
               --(composeAll . concat $
    --[ [ className =? c --> doFloat | c <- floats] ,
      --[ className =? w --> moveTo "web" | w <- webs] ,
      --[ className =? "slack"         --> moveTo "im"
        --] ])
      --where floats = ["MPlayer", ".", "feh"]
            --webs   = ["google-chrome-bin", "chromium-browser", "chromium", "chrome"]
            --moveTo = doF . W.shift

myManageHook = composeAll
    [ className =? "Chromium"  --> doShift "web"
    , className =? "Firefox"   --> doShift "web"
    , className =? "GitKraken" --> doShift "8"
    --, resource  =? "desktop_window" --> doIgnore
    {-, className =? "Steam"          --> doFloat-}
    --, className =? "stalonetray"    --> doIgnore
    --, className =? "slack"          --> doShift "social"
    , isFullscreen --> doFullFloat
    ]

--------------------------------------------
-- key bindings

myModMask = mod4Mask

myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
  spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myLock.
  , ((modMask .|. controlMask, xK_l),
     spawn myLock)

  -- Lock the screen using command specified by myHalfLock
  , ((modMask .|. controlMask, xK_x),
     spawn myHalfLock)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)
  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask .|. shiftMask, xK_s),
     spawn mySshLauncher)
  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)
  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)
  -- Take a full screenshot using the command specified by myScreenshot - with a delay.
  , ((modMask .|. controlMask .|. shiftMask, xK_l),
     spawn myDelayedScreenshot)
  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle")
  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "amixer -q set Master 5%-")
  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "amixer -q set Master 5%+")
  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")
  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")
  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")
  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

--------------------------------------------------------------------
-- "Standard" xmonad key bindings
--

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io exitSuccess)

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)

  -- switch between dynamic workspaces
  , ((modMask, xK_v), selectWorkspace def)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  [((m .|. modMask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  ++

  [((m .|. modMask, k), f sc)
      | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

--------------------------------------------
-- Startup hook

myStartupHook :: X()
myStartupHook =
    spawn "/home/evanjs/.screenlayout/default.sh && trbg"


xmobarCreator :: Bars.DynamicStatusBar
xmobarCreator (S sid) = spawnPipe $ "xmobar -x " ++ show sid

xmobarDestroyer :: Bars.DynamicStatusBarCleanup
xmobarDestroyer = return ()

--------------------------------------------
xmobarPP' = xmobarPP {
    ppSort = mkWsSort getXineramaPhysicalWsCompare
}
    where dropIx wsId = if ':' `elem` wsId then drop 2 wsId else wsId
--------------------------------------------
-- config
--

evanjsConfig = def { 
    terminal    = "kitty"
  , manageHook  = myManageHook <+> manageDocks
  , modMask     = myModMask
  , logHook     = Bars.multiPP xmobarPP' xmobarPP'
  --, workspaces  = withScreens (countScreens) myWorkspaces
  , workspaces  = myWorkspaces
  , layoutHook  = myLayouts
  , startupHook = myStartupHook
  , keys        = myKeys
  }

main = do
  xmonad . docks =<< xmobar evanjsConfig
