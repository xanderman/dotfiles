import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Dzen
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Map (fromList)
import Data.Monoid (mappend)
import Data.Ratio ((%))
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified LocalMods

role = stringProperty "WM_WINDOW_ROLE"
myLayout = noBorders Full ||| Tall 1 0.03 0.5 ||| Mirror (Tall 1 0.03 0.5)
{- myLayout = onWorkspace "8" (layoutHintsWithPlacement (0,0) $ withIM (1%7) (Title "Hangouts") $ Grid) -}
           {- $ (Full ||| Tall 1 0.03 0.5 ||| Mirror (Tall 1 0.03 0.5)) -}

myManageHook = composeAll
        {- Use xprop to find the WM_CLASS for 'className' matches -}
        [ transience'
        , isDialog                                            --> doCenterFloat
        , role =? "pop-up"                                    --> doCenterFloat
        {- Chat windows don't steal focus and show up on all workspaces -}
        , appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd"   --> (doF W.focusDown <+> doCopyToAll)
        , className =? "Gnobots2"                             --> doFloat
        , className =? "Gimp"                                 --> doFloat
        , className =? "Calculator"                           --> doFloat
        , className =? "Lightsoff"                            --> doFloat
        , title =? "jDip"                                     --> doFloat
        , title =? "Event Tester"                             --> doFloat
        -- , stringProperty "WM_COMMAND" =? "xev"                --> doFloat
        ] <+> manageDocks <+> manageHook defaultConfig
        where
            doCopyToAll = ask >>= doF . \w -> (\ws -> foldr($) ws (map (copyWindow w) myWorkspaces))

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        [ ((m .|. modm, k), windows $ f i)
            | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]] ++
        [ ((modm .|. shiftMask, xK_c), kill1)
        , ((modm .|. shiftMask, xK_0), windows copyToAll) -- mod+shit+0 to make window always visible
        , ((modm, xK_0), killAllOtherCopies) -- mod+0 to undo
        , ((modm, xK_a), goToSelected defaultGSConfig)
        , ((modm, xK_s), spawnSelected defaultGSConfig ["eclipse46_testing","google-chrome-beta","gvim","xterm"])
        ]

centered = onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]
myAlert = dzenConfig centered . show . round

myChannels = ["Master", "Headphone", "Speaker", "PCM"]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmh defaultConfig
        { manageHook = myManageHook
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , layoutHook = avoidStruts  $  myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" ""
                        }
        , modMask = mod4Mask    -- Rebind Mod to the Windows key
        , terminal = "urxvt"    -- Use urxvt terminal
        , borderWidth = 4
        , normalBorderColor = "#cccccc"
        , focusedBorderColor = "#cd8b00"
        , workspaces = myWorkspaces
        , keys = myKeys <+> keys defaultConfig
        } `additionalKeysP` (
        [ ("C-<Print>", safeSpawn "gnome-screenshot" ["-a"])
        , ("<Print>", safeSpawn "gnome-screenshot" [])
        , ("<XF86AudioMute>", toggleMuteChannels myChannels >> return ())
        , ("<XF86AudioLowerVolume>", setMuteChannels myChannels False >> lowerVolumeChannels myChannels 4 >>= myAlert)
        , ("<XF86AudioRaiseVolume>", setMuteChannels myChannels False >> raiseVolumeChannels myChannels 4 >>= myAlert)
        , ("<XF86MonBrightnessUp>", safeSpawn "xbacklight" ["-inc", "20"])
        , ("<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-dec", "20"])
        ] ++ LocalMods.additionalKeysP)
