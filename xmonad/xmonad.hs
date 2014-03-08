import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
-- import XMonad.Actions.Volume
import System.IO
import qualified XMonad.StackSet as W

main = do
  xmproc <- spawnPipe "xmobar /home/cameron/.xmonad/xmobar.hs"

  xmonad $ defaultConfig
      { manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = avoidStruts  $  layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "green" "" . shorten 50
      }
      -- Rebind Mod to the Windows key
      , modMask = mod4Mask
      , terminal = "/usr/bin/urxvt -rv"
      , focusedBorderColor = myFocusedBorderColor
      } `additionalKeys`
      [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        -- Volume Down
      , ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer -c 0 set Master 2dB-")
        -- Volume Up
      , ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer -c 0 set Master 2dB+")
        -- Brightness up
      , ((noModMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
        -- Brightness down
      , ((noModMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
        -- Mute
      -- , ((noModMask, xF86XK_AudioMute), toggleMute >> return())
        -- Spotify Launch
      , ((noModMask, xF86XK_Tools), spawn "spotify")
        -- launch dmenu
      , ((mod4Mask, xK_space), spawn "dmenu_run -b")
        -- Rotate through the available layout algorithms
      , ((mod4Mask, xK_Tab ), sendMessage NextLayout)
        -- close focused window
      , ((mod4Mask .|. shiftMask, xK_k), kill)
        -- Move focus to the next window
      , ((mod4Mask, xK_n), windows W.focusDown)
        -- Move focus to the previous window
      , ((mod4Mask, xK_p), windows W.focusUp)
        -- Swap the focused window with the next window
      , ((mod4Mask .|. shiftMask, xK_n), windows W.swapDown)
        -- Swap the focused window with the previous window
      , ((mod4Mask .|. shiftMask, xK_p), windows W.swapUp)
      --   -- Shrink the master area
      -- , ((mod4Mask, xK_f), sendMessage Shrink)
      --   -- Expand the master area
      -- , ((mod4Mask, xK_b), sendMessage Expand)
      ]

myFocusedBorderColor = "#00ffff"
