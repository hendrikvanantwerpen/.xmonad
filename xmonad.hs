import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Hooks.SetWMName
import XMonad.Layout.SimpleDecoration
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

-- Wire things into the configuration

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

-- The custom configuration

myStartupHook = setWMName "LG3D" -- make AWT work

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

hangoutsAppName = "crx_nckgahadagoaajjgafhacjanaoiihapd"
hangoutsTitle = "Hangouts"

myManageHook = composeAll $ reverse
    [ idHook
    , isFullscreen --> doFullFloat
    , className =? "Thunderbird" --> doShift "9"
    , className =? "Update-manager" --> doFloat
    , className =? "Zenity" --> doFloat
    , className =? "Gimp" --> unFloat
    , appName =? hangoutsAppName <||> appName =? "Pidgin" --> doFloat
    , appName =? hangoutsAppName <&&> title /? "Hangouts" --> placeHook myChatPlacement
    , appName =? "Pidgin" <&&> title /? "Pidgin" --> placeHook myChatPlacement
    , manageDocks
    ]

myChatPlacement = withGaps (32,32,32,32) (smart (1,1))

myLayoutHook = avoidStruts . smartBorders

myLogHook = idHook

myKeys = []

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

-- Utility functions

unFloat = ask >>= doF . W.sink

(/?) :: Eq a => Query a -> a -> Query Bool
(/?) qa a = qa =? a >>= return . not
