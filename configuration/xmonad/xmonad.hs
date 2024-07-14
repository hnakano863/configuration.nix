import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.SpawnOnce (spawnOnce)


main :: IO ()
main = xmonad . xmobarProp $ def
  { modMask = mod4Mask -- Rebind Mod to the Super key
  , terminal = "alacritty"
  , startupHook = myStartupHook
  }
  `additionalKeysP`
  [ ("M-[", spawn "emacseditor")
  , ("M-d", spawn "rofi -show drun")
  , ("M-s", spawn "rofi -show power-menu")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ("<XF86MonBrightnessDown>", spawn "light -U 10")
  , ("<XF86MonBrightnessUp>", spawn "light -A 10")
  ]
  `removeKeysP`
  [ "M-p"
  ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "trayer --edge top --align right --SetDockType true \
            \--SetPartialStrut true --expand true --width 10 \
            \--transparent true --tint 0x5f5f5f --height 18"
