--
-- xmonad.hs
-- Copyright (C) 2018 evanjs <evanjs@asentoo>
--
-- Distributed under terms of the MIT license.
--

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import System.IO

import qualified Data.Map as M

myRofi = "rofi -modi -lines 7 -show run -columns 2"
myPP = xmobarPP { ppCurrent = xmobarColor "green" "" . wrap "<" ">" . shorten 68}
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = defaultConfig
		{ modMask = mod4Mask
		, terminal = "kitty"
		} `additionalKeysP`
		[ ("M-d", spawn myRofi)
		]

myBar = "taffybar"


main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig 
