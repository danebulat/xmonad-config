import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Util.Run

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed

-- EVMH hooks (talk to window server and compositor)
import XMonad.Hooks.EwmhDesktops

-- Xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

-- For default floating windows
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

-- -------------------------------------------------------------------------------------------------
-- Manage Hook (floating windows)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp"       --> doFloat
    , className =? "Galculator" --> doFloat
    , className =? "Nemo"       --> doFloat
    , isDialog                  --> doFloat
    ]

-- -------------------------------------------------------------------------------------------------
-- Layouts

mySpacing = spacingRaw False  -- False; apply even with single window
           (Border 5 5 5 5)   -- Screen border size
           True               -- Enable screen border
           (Border 5 5 5 5)   -- Window border size
           True               -- Enable window border size

myLayoutA = Tall 1 (3/100) (1/2)
            ||| Mirror (Tall 1 (3/100) (3/5))
            ||| Full
--          ||| Grid ||| spiral (6/7)

myLayoutB = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = renamed [Replace "ThreeCol"] $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1       -- Default number of windows in the master pane
    ratio    = 1/2     -- Default proportion of screen occupied by master pane
    delta    = 3/100   -- Percent of screen to increment by when resizing panes

-- -------------------------------------------------------------------------------------------------
-- Borders

myBorderWidth = 1
myNormalBorderColor = "dddddd"
myFocusedBorderColor = "#007acc"

-- Terminal
myTerminal :: String
myTerminal = "gnome-terminal"

-- -------------------------------------------------------------------------------------------------
-- Xmobar PP

myXmobarPP :: PP
myXmobarPP = def
    { ppSep           = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent       = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden        = white . wrap " " ""
    , ppUrgent        = red . wrap (yellow "!") (yellow "!")
    , ppOrder         = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras        = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

-- Windows should have *some* title, which should not exceed a sane length.
ppWindow :: String -> String
ppWindow = xmobarRaw . (\w -> if null w then "untited" else w) . shorten 30

blue, lowWhite, magenta, red, white, yellow :: String -> String
magenta  = xmobarColor "#ff79c6" ""
blue     = xmobarColor "#bd93f9" ""
white    = xmobarColor "#f8f8f2" ""
yellow   = xmobarColor "#f1fa8c" ""
red      = xmobarColor "#ff5555" ""
lowWhite = xmobarColor "#bbbbbb" ""

-- -------------------------------------------------------------------------------------------------
-- Main

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     -- replace xmobarPP with myXmobarPP for another style
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure xmobarPP)) defToggleStrutsKey
     $ myConfig
     where
       toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
       toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConfig = def
     { modMask            = mod4Mask  -- Rebind Mod to the Super key
     ,  terminal           = myTerminal
     , borderWidth        = myBorderWidth
     , normalBorderColor  = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , layoutHook         = mySpacing $ myLayoutB
     , manageHook         = myManageHook
     }

     `additionalKeysP`
       [ ("M-S-z", spawn "slock"             )
       , ("M-S-=", unGrab *> spawn "scrot -s")
       , ("M-S-b", spawn "brave"             )
       ]

