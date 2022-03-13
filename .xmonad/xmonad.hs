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

-- Default / Grid / Fullscren without borders
smartLayout = Tall 1 (3/100) (55/100) ||| Grid ||| noBorders (fullscreenFull Full)

-- smartBorders removes borders when single window in workspace
noStrutLayout = smartBorders $ smartLayout
strutLayout = avoidStruts
  $ smartBorders . avoidStruts
  $ smartLayout

-- TTY workspaces never have strut (bar)
myLayoutHook =
  onWorkspace "NET" strutLayout
  $ onWorkspace "TTY1" noStrutLayout
  $ onWorkspace "TTY2" noStrutLayout
  $ onWorkspace "DEV" strutLayout
  $ strutLayout

myWorkspaces =
  [ "NET", "TTY1", "TTY2"
  , "DEV"
  , "COM"
  , "6", "7", "8", "9"
  ]

myModmask = mod4Mask -- ralt
myTerminal = "alacritty"

-- necessary?
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
  -- xmproc <- spawnPipe "xmobar"
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
    , handleEventHook = handleEventHook defaultConfig
                               <+> docksEventHook
                               <+> fullscreenEventHook
    -- , logHook = dynamicLogWithPP xmobarPP
                        -- { ppOutput = hPutStrLn xmproc
                        -- , ppTitle = xmobarColor "green" "" . shorten 50
                        -- }
    , startupHook = do
        setWMName "LG3D" <+> setFullscreenSupported
        -- trayer must run AFTER xmonad has started or it will sit behind any windows
        -- spawn "/usr/bin/polybar example"
        spawn "/usr/bin/trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l --height 32 --iconspacing 4 --expand false"
        spawnOn "NET" "firefox"
        spawnOn "TTY1" myTerminal
        spawnOn "TTY2" myTerminal
        spawnOn "COM" "telegram-desktop"
        spawnOn "COM" "chromium"
        sendMessage ToggleStruts -- hide strut on first workspace
    }
    `additionalKeys`
      [ ((myModmask, xK_p), spawn "dmitri") -- launcher
      -- , ((myModmask, xK_p), spawn "dmenu_run -fn 'ProFontWindows-12' -sb '#f0e68c' -sf black -nf '#f0e68c' -nb black")
      , ((myModmask .|. shiftMask, xK_m), io (exitWith ExitSuccess)) -- quit?
      , ((myModmask, xK_b), sendMessage ToggleStruts) -- toggle struts (bar)
      ]

