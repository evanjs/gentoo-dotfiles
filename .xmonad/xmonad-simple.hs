--
-- xmonad.hs
-- Copyright (C) 2018 evanjs <evanjs@asentoo>
--
-- Distributed under terms of the MIT license.
--

module Main where

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.Taffybar.Support.PagerHints (pagerHints)

import System.IO

import qualified Data.Map as M

myRofi = "rofi -modi -lines 7 -show run -columns 2"
--toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = defaultConfig
       { modMask = mod4Mask
       , terminal = "kitty"
       } `additionalKeysP`
       [ ("M-d", spawn myRofi)
       ]

main = xmonad $
       docks $
       ewmh $
       pagerHints
       myConfig

