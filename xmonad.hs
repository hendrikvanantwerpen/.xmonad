import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Hooks.SetWMName
import XMonad.Layout.SimpleDecoration
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

main = do
    xmonad $ gnomeConfig
        { startupHook = myStartupHook
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , layoutHook = myLayoutHook $ layoutHook gnomeConfig
        , logHook = myLogHook <+> logHook gnomeConfig
        , workspaces = myWorkspaces
        , modMask = mod4Mask
        , handleEventHook = fullscreenEventHook
        , borderWidth = 2
        }
        `additionalKeysP` myKeysP
        `additionalKeys` myKeys

myStartupHook = setWMName "LG3D" -- make AWT work

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myManageHook = composeAll
    [ manageDocks
    , className =? "Thunderbird" --> doShift "9"
    , className =? "Update-manager" --> doFloat
    , className =? "Zenity" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myLayoutHook = avoidStruts . smartBorders

myLogHook = return ()

myKeys =
    [
    ]

myKeysP =
    [ ("M-S-q", spawn "gnome-session-quit --power-off")
    , ("M-S-x", spawn "xmodmap ~/.Xmodmap")
    , ("M-S-<Space>", swapNextScreen)
    , ("M-<Up>", prevWS)
    , ("M-<Down>", nextWS)
    , ("M-S-<Up>", shiftToPrev >> prevWS)
    , ("M-S-<Down>", shiftToNext >> nextWS)
    , ("M-<Left>", nextScreen)
    , ("M-<Right>", prevScreen)
    , ("M-S-<Left>", shiftNextScreen >> nextScreen)
    , ("M-S-<Right>", shiftPrevScreen >> prevScreen)
    , ("M-z", toggleWS)
    ] ++
    [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.view)
                                   , ("S-", windows . W.shift) ]
    ]
