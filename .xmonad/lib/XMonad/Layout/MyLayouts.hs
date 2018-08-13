--
-- window-rules.hs
-- Copyright (C) 2018 evanjs <evanjs@asentoo>
--
-- Distributed under terms of the MIT license.
--

{-module Main where-}
module XMonad.Layout.MyLayouts where

import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

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
	    [ className =? "chromium-browser-chromium" --> doShift "web"
	    , className =? "Firefox"  --> doShift "web"
	    , resource  =? "desktop_window" --> doIgnore
	    , className =? "Steam"          --> doFloat
	    , className =? "stalonetray"    --> doIgnore
	    , className =? "slack"          --> doShift "social"
	    , isFullscreen --> doFullFloat
	    ]
	    {-, isFullscreen --> (doF W.focusDown <+> doFullFloat)]-}


