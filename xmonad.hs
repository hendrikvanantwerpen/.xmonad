import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders (smartBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

main = do
    xmonad $ gnomeConfig
        { manageHook = myManageHook <+> manageHook gnomeConfig
        , layoutHook = myLayoutHook $ layoutHook gnomeConfig
        , logHook = myLogHook <+> logHook gnomeConfig
        , modMask = mod4Mask
        , handleEventHook = fullscreenEventHook
        , borderWidth = 2
        }
        `additionalKeysP` myKeysP
        `additionalKeys` myKeys

myManageHook = composeAll
    [ manageDocks
    , className =? "Mail" --> doShift "9"
    , className =? "Update-manager" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myLayoutHook =
    avoidStruts
    . smartBorders

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
    ]
