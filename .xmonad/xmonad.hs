import XMonad
import XMonad.Config.Xfce
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Grid
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import System.Exit
import System.IO

myLayouts = avoidStruts
  $ smartBorders
  $ Tall 1 (3/100) (3/5) ||| Grid

myLayoutHook = onWorkspace "sink" (simpleFloat ||| Grid) $ myLayouts

myWorkspaces =
  [ "NET", "TTY1", "TTY2"
  , "DEV"
  , "COM"
  , "6", "7", "8", "9"
  ]

myModmask = mod4Mask
myTerminal = "kitty"

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

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar"
  xmonad $ defaultConfig
    { terminal = myTerminal
    , borderWidth = 2
    , normalBorderColor = "#392b00"
    , focusedBorderColor = "#26F972"
    , modMask = myModmask
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> manageSpawn
                               <+> manageHook defaultConfig
    , logHook    = myLogHook xmproc
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
    -- , startupHook = do
       -- startupHook defaultConfig
       -- spawn "trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l"
    }
    `additionalKeys`
      [ ((myModmask, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
      -- , ((myModmask .|. shiftMask, xK_q), spawn "xfce4-session-logout")
      , ((myModmask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
      , ((myModmask .|. shiftMask, xK_l), do
          spawn "trayer --edge top --align right --SetPartialStrut true --transparent true --tint 0x000000 -l"
          spawnOn "NET" "firefox"
          spawnOn "TTY1" myTerminal
          spawnOn "TTY2" myTerminal
          spawnOn "DEV" "chromium"
          spawnOn "COM" "telegram-desktop"
          spawnOn "COM" "slack"
        )
      ]
      -- ++
      -- [((m .|. modMask, k), windows $ f i)
        -- | (i, k) <- zip (XMonad.workspaces xfceConfig) [xK_1 .. xK_9]

