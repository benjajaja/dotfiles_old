import Control.Monad
import Data.Maybe
import Data.List
import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import System.Exit
import System.IO

smartLayout = Tall 1 (3/100) (3/5) ||| Grid ||| noBorders (fullscreenFull Full)

myLayouts = avoidStruts
  $ smartBorders . avoidStruts
  $ smartLayout

myLayoutHook =
  onWorkspace "TTY1" (smartBorders $ smartLayout )
  $ onWorkspace "TTY2" (smartBorders $ smartLayout )
  $ onWorkspace "DEV" (smartBorders $ smartLayout )
  $ myLayouts

myWorkspaces =
  [ "NET", "TTY1", "TTY2"
  , "DEV"
  , "COM"
  , "6", "7", "8", "9"
  ]

myModmask = mod4Mask
myTerminal = "alacritty"

myLogHook h =
    dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn h
                        , ppTitle = xmobarColor "#888888" "" . shorten 50
                        , ppLayout = wrap "[" "]"
                        , ppCurrent = xmobarColor "#000000" "#26f972" . id
                        , ppVisible = xmobarColor "#000000" "#cccccc" . id
                        , ppVisibleNoWindows = Just $ xmobarColor "#000000" "#cccccc" . id
                        , ppHidden = id
                        , ppHiddenNoWindows = xmobarColor "#cccccc" "" . id
                        , ppSep = " "
                        }

setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
      supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
      changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)


main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ defaultConfig
    { terminal = myTerminal
    , borderWidth = 4
    , normalBorderColor = "#392b00"
    , focusedBorderColor = "#f0e68c"
    , modMask = myModmask
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageSpawn
                               <+> (className =? "gnome-mplayer" --> doFullFloat)
                               <+> (isFullscreen --> doFullFloat)
                               <+> fullscreenManageHook
                               <+> manageHook defaultConfig
    , logHook    = myLogHook xmproc
    , handleEventHook = handleEventHook defaultConfig
                               <+> docksEventHook
                               <+> fullscreenEventHook
    , startupHook = do
        setWMName "LG3D" <+> setFullscreenSupported
        -- start trayer with delay so that it goes over xmobar
        spawn "(sleep 1; /usr/bin/trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l)"
        -- scale output together with DPI of 192 in .Xresources
        spawn "xrandr --output eDP1 --scale 1.5x1.5"

        -- repeat
        spawn "xset b 100 0 0"
        spawn "xset r rate 200 40"
        -- special keys
        spawn "setxkbmap -option caps:escape -option altwin:swap_lalt_lwin -layout us altgr-intl"


        -- media keys
        spawn "xbindkeys"

        -- tray icons
        spawn "nm-applet"
        spawn "volumeicon"
        -- spawn "gtrayicon --activate=\"xinput float $(keyboardid)\" --deactivate=\"xinput reattach $(keyboardid) 3\""

        -- screensaver and lock
        spawn "xscreensaver"
        spawn "xss-lock xscreensaver-command --lock"

        -- desktop background
        spawn "hsetroot -center $HOME/Pictures/skull_on_fire_framed_c700-477x480.j"

        -- spawn default apps
        spawnOn "NET" "firefox"
        spawnOn "TTY1" myTerminal
        spawnOn "TTY2" myTerminal
        spawnOn "DEV" "chromium"
        spawnOn "COM" "telegram"
    }
    `additionalKeys`
      [ ((myModmask, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
      , ((myModmask .|. shiftMask, xK_m), io (exitWith ExitSuccess))
      , ((myModmask .|. shiftMask, xK_l), do
          spawnOn "NET" "firefox"
          spawnOn "TTY1" myTerminal
          spawnOn "TTY2" myTerminal
          spawnOn "DEV" "chromium"
          spawnOn "COM" "telegram"
        )
      , ((myModmask, xK_b), sendMessage ToggleStruts)
      ]

