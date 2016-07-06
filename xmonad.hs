import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders ( noBorders, smartBorders)
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ToggleLayouts
--import XMonad.Hooks.EwmhDesktops --ambigous with fullscreenEventHook
import Graphics.X11.ExtraTypes.XF86
import System.IO
import Data.Ratio ((%))

-- molokai color scheme
-- To Do: document which color scheme is used and where it is located to prepare for easy changes
-- dark grey:   #272822
-- pink:        #F92672
-- blue:        #66D9EF
-- green:       #A6E22E
-- ocker:       #FD971F


-- mod keys:
-- mod1mask -> [alt left]
-- mod4mask -> [super]

-- VARIABLES FOR XMOBAR
myTitleColor        =   "#272822"
myTitleLength       =   40
myCurrentWSColor    =   "#F92672"
myVisibleWSColor    =   "#66D9EF"
--myUrgentWSColor     =   "#F92672"
myUrgentWSColor     =   "#F92672"
myUrgentWSLeft      =   "!"
myUrgentWSRight     =   "!"
myVisibleWSLeft     =   "("
myVisibleWSRight    =   ")"
myCurrentWSLeft     =   "["
myCurrentWSRight    =   "]"


myManageHook = floatHook <+> fullscreenManageHook
floatHook = composeAll
    [ className =? "Gimp"   --> doFloat
    , resource =? "synapse" --> doFloat
    , resource =? "gimp" --> doFloat
    , resource =? "virtualbox" --> doFloat
    , resource =? "keepassx" --> doFloat
    , resource =? "gnome-calendar" --> doFloat]

myStartupHook ::X ()
myStartupHook = do
    spawn "compton -f -I 0.10 -O 0.10 --backend glx --vsync opengl"

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig { 
        manageHook      = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , startupHook   = myStartupHook <+> startupHook defaultConfig
        , handleEventHook   =   handleEventHook defaultConfig <+> fullscreenEventHook
        , layoutHook    = avoidStruts $ toggleLayouts (noBorders Full) $ smartBorders $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#F92672" "#272822" . shorten myTitleLength
                        , ppCurrent =   xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight
                        , ppVisible =   xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight 
                        , ppUrgent =   xmobarColor myUrgentWSColor "" . wrap myUrgentWSLeft myUrgentWSRight 
                        }
        , modMask                 = myModMask
        , terminal                = myTerminal
        , workspaces              = myWorkSpaces
    
        --appearance
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "physlock -ds")
        , ((mod1Mask        , xK_space), spawn "/home/timon/dotfiles/bin/layout_switch")
        , ((mod4Mask            , xK_m), sendMessage ToggleStruts)
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +2db")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -2db")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
        , ((controlMask .|. mod1Mask, xK_t), spawn "urxvt")
        , ((controlMask, xK_space), spawn "synapse")
        , ((0, xF86XK_Tools), spawn "systemctl suspend")
        , ((0, 0x1008FF21), spawn "systemctl suspend")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((0, xK_Insert), pasteSelection) -- there is a problem here, as it seems to escape some characters
        ]

myTerminal              = "urxvt"
myModMask               = mod4Mask -- [super]
myBorderWidth           = 1
myNormalBorderColor     = "#e0e0e0"
myFocusedBorderColor    = "#F92672"
myWorkSpaces    = [ "1:web", "2:term", "3:music", "4:work1", "5:work2", "6:writing", "7:other" ]
