import XMonad
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.ManageHook
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Paste
import XMonad.Util.NamedWindows
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders(noBorders, smartBorders)
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops --ambigous with fullscreenEventHook
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
myTitleLength       =   30
myCurrentWSColor    =   "#F92672"
myVisibleWSColor    =   "#66D9EF"
myUrgentWSColor     =   "#F92672"
myUrgentWSLeft      =   "!"
myUrgentWSRight     =   "!"
myVisibleWSLeft     =   "("
myVisibleWSRight    =   ")"
myCurrentWSLeft     =   "["
myCurrentWSRight    =   "]"

--TODO clarify the urgencyHook does not seem to be used. Maybe clean this up OR work out what I wanted to achieve here
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name        <- getName w
        Just idx    <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace" ++ idx]

myManageHook = floatHook <+> fullscreenManageHook <+> namedScratchpadManageHook scratchpads

floatHook = composeAll
    [ className =? "gimp"   --> doFloat
    , resource =? "synapse" --> doIgnore
    , resource =? "arandr" --> doFloat
    , resource =? "keepassx2" --> doFloat
    , resource =? "skype" --> doFloat
    , resource =? "gnome-calendar" --> doFloat
    , resource =? "gnome-control-center" --> doFloat
    , resource =? "gnome-weather" --> doFloat]

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "terminal" spawnTerminal findTerminal manageTerminal
        , NS "keepassxc" spawnKeepassxc findKeepassxc manageKeepassxc
        , NS "spotify" spawnSpotify findSpotify manageSpotify
        , NS "gvim" spawnGvim findGvim manageGvim
        , NS "streamdeck" spawnStreamdeck findStreamdeck manageStreamdeck
        ]
        where
            spawnTerminal   = myTerminal ++ " -name scratchpad"
            findTerminal    = resource =? "scratchpad"
            manageTerminal  = customFloating $ W.RationalRect l t w h
                        where
                            h = 0.9
                            w = 0.9
                            t = 0.05
                            l = 0.05
            spawnKeepassxc  = "keepassxc"
            findKeepassxc   = className =? "KeePassXC"
            manageKeepassxc = customFloating $ W.RationalRect l t w h
                        where
                            h = 0.6
                            w = 0.6
                            t = 0.1
                            l = 0.1
            spawnSpotify    = "spotify"
            findSpotify     = className =? "Spotify"
            manageSpotify   = customFloating $ W.RationalRect l t w h
                        where
                            h = 0.9
                            w = 0.9
                            t = 0.15
                            l = 0.15
            spawnGvim       = "gvim"
            findGvim        = className =? "Gvim"
            manageGvim      = customFloating $ W.RationalRect l t w h
                        where
                            h = 1.1
                            w = 1
                            t = 0.02
                            l = 0.003
            spawnStreamdeck     = "streamdeck"
            findStreamdeck      = className =? "StreamDeck UI"
            manageStreamdeck    = customFloating $ W.RationalRect l t w h
                        where
                            h = 0.7
                            w = 0.7
                            t = 0.1
                            l = 0.1


--The startupHook can be used to launch programs automatically.
--I use it for picom
--Main setup is handled via ~/.xprofile now
myStartupHook ::X ()
myStartupHook = do
     spawnOnce "picom -f -I 0.10 -O 0.10 --config ${HOME}/dotfiles/picom.conf"
     --spawn "feh --no-fehbg --bg-fill ${HOME}/backgrounds/background.jpg"

main = xmonad . ewmhFullscreen =<< statusBar myBar myPP toggleStrutsKey myConfig
--withUrgencyHook LibNotifyUrgencyHook <- This still is ToDo!

--TODO use example here: https://xmonad.org/configurations.html
--to filter out NSP scratchpads
myPP = xmobarPP
            { ppCurrent = xmobarColor "#F92672" "" . wrap "[" "]"
            , ppTitle = xmobarColor "#F92672" "#272822" . shorten myTitleLength}

--myPP = xmobarPP
--             { ppTitle = xmobarColor "#F92672" "#272822" . shorten myTitleLength
--             , ppCurrent =   xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight
--             , ppVisible =   xmobarColor myVisibleWSColor "" . wrap myVisibleWSLeft myVisibleWSRight
--             , ppUrgent  =   xmobarColor myUrgentWSColor  "" . wrap myUrgentWSLeft myUrgentWSRight
--             }

myBar = "xmobar ~/dotfiles/xmobarrc"

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_m)

myConfig = def {
        manageHook      = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> myManageHook <+> manageHook def
        , startupHook   = myStartupHook <+> startupHook def --TODO
        , handleEventHook   =  handleEventHook def <+> XMonad.Layout.Fullscreen.fullscreenEventHook
        , layoutHook    = avoidStruts $ toggleLayouts (noBorders Full) $ smartBorders $ layoutHook def
        , modMask                 = myModMask
        , terminal                = myTerminal
        , XMonad.workspaces       = myWorkSpaces

        --appearance
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        } `additionalKeys`
        ([ ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock --image=${HOME}/backgrounds/background-corrupted.png") --FIXME
        , ((mod1Mask        , xK_space), spawn "${HOME}/dotfiles/bin/layout_switch")
        , ((mod4Mask            , xK_m), sendMessage ToggleStruts)                              --toggle xmobar
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +4%")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -4%")
        , ((mod1Mask.|. shiftMask, xK_j), spawn "pactl set-sink-volume @DEFAULT_SINK@ -4%")
        , ((mod1Mask.|. shiftMask, xK_k), spawn "pactl set-sink-volume @DEFAULT_SINK@ +4%")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ((controlMask, xK_i), spawn "spotifyscript notify-songinfo")                          --print songinfo via notify-send
        , ((controlMask .|. mod1Mask, xK_s), spawn "spotifyscript copyUrl")                     --copy url into copy&paste buffer
        , ((controlMask .|. mod1Mask, xK_t), spawn myTerminal)
        , ((mod1Mask .|. shiftMask, xK_comma), namedScratchpadAction scratchpads "terminal")    --urxvt quake-style
        , ((mod1Mask, xK_p), namedScratchpadAction scratchpads "keepassxc")                     --keepassxc
        , ((mod1Mask, xK_m), namedScratchpadAction scratchpads "spotify")
        , ((mod1Mask, xK_v), namedScratchpadAction scratchpads "gvim")                          --TODO FIXME this overlaps with emacs shortcut!
        , ((mod1Mask .|. shiftMask, xK_s), namedScratchpadAction scratchpads "streamdeck")
        , ((controlMask, xK_space), spawn myLauncher)
        , ((0, xF86XK_Tools), spawn "systemctl suspend")
        , ((mod1Mask, xK_Num_Lock), spawn "log-working-hours SUSPEND && systemctl suspend")
        , ((0, 0x1008FF21), spawn "log-working-hours SUSPEND && systemctl suspend")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((controlMask, xK_F3), spawn "flameshot gui")
        , ((0, xK_Print), spawn "scrot")
        , ((mod1Mask .|. shiftMask, xK_l), spawn "playerctl next")
        , ((mod1Mask .|. shiftMask, xK_h), spawn "playerctl previous")
        , ((mod1Mask .|. shiftMask, xK_space), spawn "playerctl play-pause")
        , ((0, xK_Insert), pasteSelection) -- there is a problem here, as it seems to escape some characters
        ]
        ++
        [
        ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0,1,2] --adjust to match screen order
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ]
        )


--check  https://bbs.archlinux.org/viewtopic.php?id=116266 for thinking about how to use AltGr as mod5Mask
myLauncher              = "rofi -show combi"     --TODO explore rofi
myTerminal              = "urxvt -ls" --spawn urxvt as a login_shell and parse ~/.bash_profile
myModMask               = mod4Mask -- [super]
myBorderWidth           = 1
myNormalBorderColor     = "#e0e0e0"
myFocusedBorderColor    = "#F92672"
--TODO use icons/symbols for workspaces to save some space
myWorkSpaces    = [ "1:\xfa9e", "2:_$", "3:msc", "4:@", "5:\xf738", "6:calc", "7:misc", "8:\xf9c6", "9:misc"]
