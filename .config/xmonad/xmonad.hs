import Keys

import System.Environment
-- xmonad
import XMonad
-- xmonad-contrib
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IfMax
import XMonad.Layout.Tabbed as T
-- import XMonad.Actions.UpdatePointer

startup :: X ()
startup = do
  home <- liftIO $ getEnv "HOME"
  spawn $ home ++ "/.config/xmonad/autostart.sh"

myManageHook :: ManageHook
myManageHook = composeAll
  . concat $
  [ [className =? "Virt-manager" --> doShift "V"]
  , [title =? "Picture-in-Picture" --> doFloat]
  ] 

main :: IO ()
main =
  xmonad $ ewmh $ desktopConfig
  { modMask            = globalModMask
  , terminal           = term
  , layoutHook         = myLayouts
  , startupHook        = startup
  , workspaces         = workspaceNames
  , keys               = normalKeys
  , borderWidth        = 2
  , normalBorderColor  = "#934674"
  , focusedBorderColor = "#40639f"
  -- , logHook            = updatePointer (0.5, 0.5) (0, 0)
  , clickJustFocuses   = False
  , focusFollowsMouse  = False
  , manageHook = myManageHook <> manageHook desktopConfig
  }
    where 
      tabbedLayoutTheme = T.Theme {
        inactiveColor       = "#2d1523"
      , activeColor         = "#e18bbe"
      , urgentColor         = "#40639f"
      , activeBorderColor   = "#934674"
      , inactiveBorderColor = "#e18bbe"
      , urgentBorderColor   = "#2d1523"
      , activeBorderWidth   = 1
      , inactiveBorderWidth = 1
      , urgentBorderWidth   = 1
      , activeTextColor     = "#2d1523"
      , inactiveTextColor   = "#e18bbe"
      , urgentTextColor     = "#e18bbe"
      , fontName            = "xft:JetBrains Mono:size=6"
      , decoWidth           = 200
      , decoHeight          = 16
      , windowTitleAddons   = []
      , windowTitleIcons    = []
      }
      myLayouts =
        gaps [(U,26)] $ tallGrid
        -- ||| Grid         
        ||| tiled        
        ||| Mirror tiled 
        ||| tabby 
        -- ||| Full
        where
          tiled = Tall nmaster delta ratio
          tallGrid = IfMax 2 (tiled) Grid
          nmaster = 1   -- Default # in master
          ratio = 1/2   -- Size master:stack
          delta = 3/100 -- Amount to resize
          tabby = tabbedBottomAlways shrinkText tabbedLayoutTheme
