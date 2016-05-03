module LXDE (lxdeConfig) where

import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run

import qualified Data.Map as M

lxdeConfig = desktopConfig
    { keys = lxdeKeys <+> keys desktopConfig
    }

lxdeKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), spawn "lxpanelctl run")
    , ((modm .|. shiftMask, xK_q), spawn "lxsession-logout") ]
