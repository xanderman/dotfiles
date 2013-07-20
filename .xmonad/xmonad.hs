import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

role = stringProperty "WM_WINDOW_ROLE"

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaultConfig
        { manageHook = composeAll
          {- [ className =? "Google-chrome" --> doFloat -}
          [ role =? "pop-up" --> doFloat
          , className =? "Gnobots2"      --> doFloat
          , className =? "Gimp"          --> doFloat
          , className =? "Calculator"    --> doFloat
          ] <+> manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask    -- Rebind Mod to the Windows key
        , borderWidth = 4
        , normalBorderColor = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        } `additionalKeysP`
        [ ("M1-C-l", spawn "xscreensaver-command -lock")
        , ("C-<Print>", spawn "gnome-screenshot -a")
        , ("<Print>", spawn "gnome-screenshot")
        , ("<XF86AudioMute>", spawn "amixer -q -c 2 set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q -c 2 set Master 4%-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q -c 2 set Master 4%+")
        ]
