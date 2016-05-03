import Data.List
import Graphics.X11.ExtraTypes.XF86
import LXDE
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Volume
import XMonad.Config.Desktop
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

-- Wire things into the configuration

main = xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ lxdeConfig
    { terminal = "terminator"
    , startupHook = myStartupHook
    , manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = myLayoutHook $ layoutHook defaultConfig
    , logHook = myLogHook <+> logHook defaultConfig
    , workspaces = myWorkspaces
    , modMask = mod4Mask
    , handleEventHook = fullscreenEventHook
    , borderWidth = 2
    }
    `additionalKeysP` myKeysP
    `additionalKeys` myKeys

-- The custom configuration

myStartupHook = setWMName "LG3D"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

hangoutsAppName = "crx_nckgahadagoaajjgafhacjanaoiihapd"

myManageHook = composeAll $ reverse
    [ idHook
    , isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    , className =? "Thunderbird" --> doShift "9"
    , className =? "Update-manager" --> doCenterFloat
    , xulApp "Firefox" --> doCenterFloat
    , xulApp "Zotero" --> doCenterFloat
    , className =? "Gimp" --> unFloat
    , appName =? "gcr-prompter" --> doCenterFloat
    , matchChat --> placeHook myChatPlacement <+> doFloat
    , manageDocks
    ]

matchChat = appName =? hangoutsAppName
            <||> appName =? "Pidgin"
            <||> className =? "Skype"

xulApp name = className =? name
              <&&> (windowRole =? "Preferences"
                    <||> windowRole =? "pref"
                    <||> title `endsWith` "Preferences")

myChatPlacement = withGaps (32,32,32,32) (smart (1,1))

myLayoutHook = avoidStruts . smartBorders

myLogHook = idHook

myKeys =
    [ ((0, xF86XK_AudioLowerVolume ), lowerVolume 3 >> return ())
    , ((0, xF86XK_AudioRaiseVolume ), raiseVolume 3 >> return ())
    , ((0, xF86XK_AudioMute        ), toggleMute    >> return ())
    ]

myKeysP =
    [ ("M-S-<Space>", swapNextScreen)
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
      , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- use W.view for no swapping
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

windowRole = stringProperty "WM_WINDOW_ROLE"

(/?) :: Eq a => Query a -> a -> Query Bool
qa /? a = qa =? a >>= return . not

endsWith :: Eq a => Query [a] -> [a] -> Query Bool
qa `endsWith` a = qa >>= return . (isSuffixOf a)
