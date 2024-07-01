import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Ratio
import Data.Monoid
import System.Exit
-- these are strings. you fuckin know that
term = "alacritty"
launcher = "rofi -show drun"
normBorderColor = "#56635f"
focuBorderColor = "#e67e80"

modm = mod1Mask
superm = mod4Mask

wrkspcs :: [String]
wrkspcs = ["hi0","s0","f0","r0","rf0"] ++ map (\w -> 'l':(show w)) [4..9]

-- https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X ()
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect 0.50 0.40 0.40 0.50 ) s)) 

mahKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
mahKeys _ = M.fromList $ 
  [ 
   ((modm, xK_Return), spawn term)
  ,((modm, xK_Tab), spawn launcher)
  ,((superm, xK_w), io exitSuccess)
  ,((modm, xK_w), kill)
  ,((modm, xK_c), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  ,((modm, xK_v), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
  ,((modm, xK_f), withFocused toggleFloat)
  ]
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip wrkspcs [xK_1 .. xK_9], (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] 
  ]

mousekbinds _ = M.fromList $ 
  [
     ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    ,((modm, button3), (\w -> focus w >> mouseResizeWindow w))
  ]
  
layohook = spacing 10 $ spiral(6/7) ||| ThreeCol 1 (3/100) (1/2) 

stuhook :: X ()
stuhook = do
  spawn "xrandr --output HDMI-1 --mode 1920x1080 --rate 144"
  spawn "feh --bg-scale ~/r.png" 

hevhook :: Event -> X All
hevhook = mempty

mhook = composeAll [
        className =? "feh" --> doFloat
       ,className =? "mpv" --> doFloat
    ]

loghook :: X ()
loghook = return ()

cmask :: EventMask
cmask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

-- | The root events that xmonad is interested in
rmask :: EventMask
rmask =  substructureRedirectMask .|. substructureNotifyMask
        .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
        .|. buttonPressMask

main :: IO ()
main =
    xmonad XConfig {
      terminal = term,
      workspaces = wrkspcs,
      focusFollowsMouse = True,
      normalBorderColor = normBorderColor,
      focusedBorderColor = focuBorderColor,
      modMask = modm,
      borderWidth = 2, 
      clickJustFocuses = True,
      clientMask = cmask,
      rootMask = rmask,         
      extensibleConf = M.empty,
      logHook = loghook,
      mouseBindings = mousekbinds,
      keys = mahKeys,
      layoutHook = layohook,
      startupHook = stuhook,
      handleEventHook = hevhook,
      manageHook = mhook,
      handleExtraArgs = \ xs theConf -> case xs of
                [] -> return theConf
                _ -> fail ("boooo")  
    }
