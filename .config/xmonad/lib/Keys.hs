module Keys
  ( normalKeys
  , term
  , workspaceNames
  , globalModMask
  ) where

import System.Exit
-- xmonad
import XMonad
-- xmonad-contrib
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.StackSet as W
-- X11
import Graphics.X11.ExtraTypes.XF86
-- collections
import qualified Data.Map as M

xpConfig :: XPConfig
xpConfig = def
  { font = "xft::JetBrains Mono:size=8"
  , height = 24
  , position = Top
  , bgColor = "#2d1523"
  , fgColor = "#e18bbe"
  , bgHLight = "#40639f"
  , fgHLight = "#934674"
  , borderColor = "#e18bbe"
  , promptBorderWidth = 1
  , alwaysHighlight = False
  }

term :: String
term = "alacritty -e tmux"

workspaceNames :: [String]
workspaceNames =
  [ "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  , "9"
  , "V"]

workspaceKeys :: [KeySym]
workspaceKeys = [xK_1..xK_9] ++ [xK_0]

toggleFloat :: Window -> X ()
toggleFloat w = windows (\s ->
  case (M.member w (floating s)) of
  True  ->
    W.sink w s
  False -> 
    W.float w
      (RationalRect (1/3) (1/4) (1/2) (4/5)) s)

globalModMask :: KeyMask
globalModMask = mod4Mask

normalKeys :: XConfig Layout -> 
  M.Map (KeyMask, KeySym) (X ())
normalKeys conf = M.fromList $
  [ ((ms,  xK_r), spawn $ "xmonad --recompile &&" ++ "xmonad --restart")
  , ((m,   xK_q), kill)
  , ((ms,  xK_q), confirmPrompt xpConfig "exit" $ io exitSuccess)

  , ((m,   xK_Return), spawn $ terminal conf)
  , ((m,   xK_d     ), spawn "dmenu_run")
  , ((m,   xK_l     ), spawn "firefox")
  , ((ms,  xK_l     ), spawn "librewolf")
  , ((mc,  xK_space ), spawn "playerctl play-pause")

  , ((0,   xF86XK_AudioRaiseVolume ), spawn "amixer sset Master 2%+,2%+")
  , ((0,   xF86XK_AudioLowerVolume ), spawn "amixer sset Master 2%-,2%-")
  , ((0,   xF86XK_AudioMute        ), spawn "amixer sset Master toggle")
  , ((0,   xF86XK_AudioMicMute     ), spawn "amixer sset Capture toggle")
  , ((0,   xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 5")
  , ((0,   xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")

  , ((m,  xK_space ), withFocused toggleFloat)

  , ((ms, xK_Return), windows shiftMaster)

  , ((m,  xK_t    ), windows focusUp)
  , ((ms, xK_t    ), windows focusDown)
  , ((m,  xK_Left ), windows focusUp)
  , ((m,  xK_Right), windows focusDown)

  , ((ms, xK_Left ), windows swapUp)
  , ((ms, xK_Right), windows swapDown)

  , ((m,  xK_comma       ), sendMessage (IncMasterN 1))
  , ((m,  xK_period      ), sendMessage (IncMasterN (-1)))
  , ((m,  xK_bracketleft ), sendMessage Shrink)
  , ((m,  xK_bracketright), sendMessage Expand)

  , ((m,  xK_Tab), sendMessage NextLayout)
  , ((ms, xK_Tab), setLayout $ XMonad.layoutHook conf)
  , ((mc, xK_Tab), refresh)
  ]
  ++
  zipWith
    newViewBind workspaceKeys workspaceNames
  ++
  zipWith
    newMoveBind workspaceKeys workspaceNames
    where
      m = globalModMask
      s = shiftMask
      c = controlMask
      ms = m.|.s
      mc = m.|.c
      newViewBind key name = ((m, key ), windows $ greedyView name)
      newMoveBind key name = ((ms, key), windows $ shift name)
