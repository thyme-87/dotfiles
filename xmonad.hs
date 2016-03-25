import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

-- molokai color scheme
-- To Do: document which color scheme is used and where it is located to prepare for easy changes
-- dark grey:   272822
-- pink:        F92672
-- blue:        66D9EF
-- green:       A6E22E
-- ocker:       FD971F


-- mod keys:
-- mod1mask -> [alt left]
-- mod4mask -> [super]

main = do
    xmproc <- spawnPipe "xmobar"


    xmonad $ defaultConfig { 
        manageHook      = manageDocks <+> manageHook defaultConfig
        , layoutHook    = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask                 = myModMask
        , terminal                = myTerminal
        , workspaces              = myWorkSpaces
    
        --appearance
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "xset dpms force off; physlock -ds")
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 1 +2db")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 1 -2db")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 1 toggle")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((0, xK_Insert), pasteSelection)
        ]
--To Do:
-- define keys shortcuts and hotkeys
-- Volume up
-- Volume down
-- Sound off
-- Suspend to ram
-- open vl
myTerminal              = "urxvt"
myModMask               = mod4Mask -- [super]
myBorderWidth           = 1
myNormalBorderColor     = "#e0e0e0"
myFocusedBorderColor    = "#F92672"
myWorkSpaces    = [ "1:web", "2:term", "3:files", "4:chat", "5:music", "6:writing", "7:other" ]
