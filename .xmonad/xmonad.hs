-- /usr/bin/env runhugs +l
--
-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- https://github.com/vicfryzel/xmonad-config

module Main where

import System.Exit
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt()
import XMonad.Prompt.Shell()
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig()
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Taffybar.Support.PagerHints (pagerHints)

{-import XMonad.Layout.MyLayouts-}
------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "kitty"

-- The command to lock the screen or show the screensaver.
--myLock = "~/.config/i3/i3lock-fancy.sh"
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
-- Location of your xmobar.hs / xmobarrc
-- myXmobarrc = "~/.xmonad/xmobar-single.hs"
myBarCfg = "~/.config/taffybar/taffybar.hs"

{-myBar = "taffybar"-}
myBar = "xmobar"
------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
--myWorkspaces = ["1:term","2:web","3:code","4:vm","5:media"] ++ map show [6..9]
myWorkspaces = ["web","2","3","4","debug","6","7","build","dashboard","social"] ++ map show [11..999]


kill8 ss | Just w <- W.peek ss = (W.insertUp w) $ W.delete w ss
         | otherwise = ss

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Chromium" --> doShift "web"
    , className =? "Firefox"  --> doShift "web"
    , resource  =? "desktop_window" --> doIgnore
    {-, className =? "Steam"          --> doFloat-}
    , className =? "stalonetray"    --> doIgnore
    , className =? "slack"          --> doShift "social"
    , isFullscreen --> doFullFloat
    ]
    {-, isFullscreen --> (doF W.focusDown <+> doFullFloat)]-}



------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

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

  -- Workspace pinning
  
  -- Pin to all workspaces
  {-, ((modMask                  , xK_a        ),-}
     {-sequence_ $ [windows $ copy i | i <- XMonad.workspaces conf])     -}

  -- remove from all but current 
  {-, ((modMask .|. controlMask  , xK_a        ),-}
     {-windows $ kill8 ) -}

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer -q set Master 5%+")

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
     io (exitWith ExitSuccess))

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
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  {-xmproc <- spawnPipe ("xmobar")-}
  {-xmproc <- spawnPipe ("/home/evanjs/.local/bin/my-taffybar")-}
  _ <- spawnPipe ("/home/evanjs/.xmonad/restart-taffybar")
  xmonad $ docks $ ewmh $ pagerHints defaults {
        logHook = dynamicLogWithPP $ def
      , manageHook = manageDocks <+> myManageHook
      , startupHook = docksStartupHook >> setWMName "LG3D"
--      , startupHook = docksStartupHook <+> setWMName "LG3D"
      {-, handleEventHook = docksEventHook-}
      }


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}



