import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimpleDecoration
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import System.IO

-- Wire things into the configuration

main = xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ gnomeConfig
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

myStartupHook = idHook

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

hangoutsAppName = "crx_nckgahadagoaajjgafhacjanaoiihapd"

myManageHook = composeAll $ reverse
    [ idHook
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , className =? "Thunderbird" --> doShift "9"
    , className =? "Update-manager" --> doCenterFloat
    , (className =? "Firefox" <&&> windowRoleIs "Preferences") --> doCenterFloat
    , className =? "Gimp" --> unFloat
    , appName =? "gcr-prompter" --> doCenterFloat
    , matchChat --> placeHook myChatPlacement <+> doFloat
    , manageDocks
    ]

matchChat = appName =? hangoutsAppName <||> appName =? "Pidgin" <||> className =? "Skype"

myChatPlacement = withGaps (32,32,32,32) (smart (1,1))

myLayoutHook = avoidStruts . smartBorders

myLogHook = idHook

myKeys = []

myKeysP =
    [ ("M-q", spawn "gnome-session-quit --logout")
    , ("M-S-q", spawn "gnome-session-quit --power-off")
    , ("M-x", spawn "xmodmap ~/.xmonad/.Xmodmap")
    , ("M-S-x", spawn "xmodmap ~/.xmonad/.Xmodmap-home")
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
    , ("M-<Backspace>", focusUrgent)
    , ("M-S-<Backspace>", clearUrgents)
    ] ++
    [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.view)
                                   , ("S-", windows . W.shift) ]
    ]

-- Notifications for urgent windows
-- as per http://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]

-- Utility functions

unFloat = ask >>= doF . W.sink

windowRoleIs role = stringProperty "WM_WINDOW_ROLE" =? role

(/?) :: Eq a => Query a -> a -> Query Bool
(/?) qa a = qa =? a >>= return . not
