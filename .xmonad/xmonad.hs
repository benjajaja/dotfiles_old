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
  onWorkspace "NET" (smartBorders $ smartLayout )
  $ onWorkspace "TTY1" (smartBorders $ smartLayout )
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
    , startupHook = do
        setWMName "LG3D" <+> setFullscreenSupported
        -- trayer must run AFTER xmonad has started or it will sit behind any windows
        spawn "/usr/bin/trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l --height 32 --iconspacing 4"

        -- spawn default apps
        spawnOn "NET" "firefox"
        spawnOn "TTY1" myTerminal
        spawnOn "TTY2" myTerminal
        spawnOn "DEV" "chromium"
        spawnOn "COM" "telegram"
    }
    `additionalKeys`
      [ ((myModmask, xK_p), spawn "dmenu_run -fn 'ProFontIIx-12' -sb orange -sf black -nf orange")
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

