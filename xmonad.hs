import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Scratchpad

import XMonad.Config.Desktop

import XMonad.Layout.Spacing

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import Graphics.X11.ExtraTypes.XF86

-- Custom xmobar PP
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- Layouts
myLayout = avoidStruts $ spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $ Tall 1 (3/100) (1/2) ||| Full

-- Floaters
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"   --> doFloat
	, className =? "Signal" --> doFloat
    , isDialog              --> doFloat
    ]

-- Main configuration
myConfig = def 
    { terminal		= "alacritty"		-- set the default terminal to Alacritty
    , layoutHook	= myLayout			-- set the preferred layout
	, modMask		= mod4Mask			-- set the mod key to the super key
	, startupHook	= myStartupHook		-- launch startup apps
	, manageHook	= myManageHook		-- manage floating windows
	, borderWidth	= 0					-- remove the default border
	}
  `additionalKeysP`
    [ ("M-f"						, spawn "firefox" )
--	, ("M-t"						, spawn scratchpadSpawnAction)
	, ("<xF86XK_AudioMute>"			, spawn "amixer -q sset Master toggle")
	, ("<xF86XK_AudioLowerVolume>"	, spawn "amixer -q sset Master 5%-")
	, ("<xF86XK_AudioRaiseVolume>"	, spawn "amixer -q sset Master 5%+")
	]

-- The main function
main :: IO ()
main = xmonad 
	 . ewmhFullscreen 
	 . ewmh 
	 . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
	 $ myConfig

myStartupHook :: X ()
myStartupHook = do
	spawnOnce "nitrogen --restore &"
	spawnOnce "~/Programs/Bitcoin/bin/bitcoind &"
