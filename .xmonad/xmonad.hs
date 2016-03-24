import           Data.Monoid              (All)
import           System.Exit
import           System.IO                (Handle, hPutStrLn)
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import           XMonad.Layout.Spacing

import           XMonad.Util.NamedScratchpad ( namedScratchpadAction , namedScratchpadManageHook
                                             , NamedScratchpad(NS), customFloating)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.EZConfig (additionalKeys)

import qualified Data.Map                 as M
import qualified XMonad.StackSet          as W

term :: String
term = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

mMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = [devel, code, web, music, media, mail, misc]
               where music = "   ^i(/home/creek/.xmonad/icons/Music.xbm)"
                     web   = "   ^i(/home/creek/.xmonad/icons/www.xbm)"
                     code  = "   ^i(/home/creek/.xmonad/icons/code.xbm)"
                     devel = "   ^i(/home/creek/.xmonad/icons/Devel.xbm)"
                     media = "   ^i(/home/creek/.xmonad/icons/media.xbm)"
                     mail  = "   ^i(/home/creek/.xmonad/icons/mail.xbm)"
                     misc  = "   ^i(/home/creek/.xmonad/icons/pacman.xbm)"

scratchpads :: [NamedScratchpad]
scratchpads =
    [ scratch "term"   ""
    , scratch "term2"  ""
    -- , scratch "ranger" " -e ranger"
    ]
    where
      scratchpadSize = W.RationalRect (1/5) (1/5) (3/5) (3/5)
      mySPFloat      = customFloating scratchpadSize
      -- Format Scratchpads
      scratch label command = NS label (term ++ " -name " ++ label ++ command)
                              (resource =? label)
                              mySPFloat
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys =
    [ ((mMask .|. shiftMask, xK_Return), spawn term)
    , ((mMask,               xK_d     ), spawn dmenu)
    , ((mMask,               xK_c     ), spawn "chromium")
    , ((mMask,               xK_x     ), spawn "emacsclient -c -a \"\"")
    , ((mMask .|. shiftMask, xK_c     ), kill)

    -- Scratchpads

    , ((mMask, xK_Return              ), namedScratchpadAction scratchpads "term")
    , ((mMask .|. shiftMask, xK_Return), namedScratchpadAction scratchpads "term2")


    , ((mMask,               xK_j     ), windows W.focusDown)
    , ((mMask,               xK_k     ), windows W.focusUp  )
    , ((mMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((mMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((mMask .|. shiftMask, xK_h     ), sendMessage Shrink)
    , ((mMask .|. shiftMask, xK_l     ), sendMessage Expand)
    , ((mMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((mMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((mMask              , xK_b     ), sendMessage ToggleStruts)

    -- Restart xmonad
    , ((mMask .|. shiftMask, xK_q     ), io exitSuccess)
    , ((mMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    where
      dmenu = concat [ "exe=`dmenu_path | ~/.local/bin/yeganesh"
                     , " -x "
                     , " -- -i -b "
                     , "-sb \"#689d6a\" "
                     , "-sf \"#2d2d2d\" "
                     , "-nb \"#2d2d2d\" "
                     , "-nf grey "
                     , "-fn 'Source Code Pro-9'` "
                     , "&& eval \"$exe\""
                     ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings
  :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig{XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    ]

myLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myManageHook :: ManageHook
myManageHook =  composeAll [
  manageDocks
  , (isFullscreen --> doFullFloat)
  , namedScratchpadManageHook scratchpads
  , manageHook def
  ]

myEventHook :: Event -> X All
myEventHook = mempty

myLogHook :: Handle -> X ()
myLogHook dzproc =
  fadeInactiveLogHook 0.9 <+>
  dynamicLogWithPP dzenPP {
      ppOutput = hPutStrLn dzproc
    , ppTitle =  pad  . shorten 50
    , ppLayout = dzenColor color4 background .
      (\x -> case x of
        "Spacing 10 Tall" -> "    ^i(/home/creek/.xmonad/icons/tiling.xbm)"
        "Spacing 10 Mirror Tall" -> "    ^i(/home/creek/.xmonad/icons/mirrortall.xbm)"
        "Spacing 10 Full" -> "    ^i(/home/creek/.xmonad/icons/mirrortall.xbm)"
        _ -> "  New Layout "
      )
    , ppCurrent = dzenColor foreground background -- foreground "#FF6000"
    , ppVisible = dzenColor color4 background
    , ppHidden = dzenColor color4 background  -- foreground "#7BB352"
    , ppHiddenNoWindows = dzenColor color8 background
    , ppOrder = \(ws:l:t:_) -> [ws,l,t]
    }

myStartupHook :: X ()
myStartupHook = setWMName "LG3D"

bar1Width = "1000"
bar2Width = "920"

myStatusBar, myConkyBar, bar1Width, bar2Width :: String
myStatusBar =
  "dzen2 -x 0 -w '"
  ++ bar1Width ++ "' -h '28' -ta l -xs 1 -fg '"
  ++ foreground ++ "' -bg '" ++ background ++ "' -fn '"
  ++ myFont ++ "'"
myConkyBar =
  "conky -c ~/.xmonad/conky_dzen | dzen2 -ta r -x '"
  ++ bar1Width ++"' -w '"
  ++ bar2Width ++ "' -h '28' -p $OPTS -xs 1 -fg '"
  ++ foreground ++ "' -bg '" ++ background
  ++ "' -fn '" ++ myFont ++ "'"

main :: IO ()
main = do
  dzproc <- spawnPipe myStatusBar
  _ <- spawnPipe myConkyBar
  xmonad $ def {
        terminal           = term,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = 0,
        modMask            = mMask,
        workspaces         = myWorkspaces,
        mouseBindings      = myMouseBindings,
        layoutHook         = smartSpacing 20 $ avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> docksEventHook,
        logHook            = myLogHook dzproc,
        startupHook        = myStartupHook
    } `additionalKeys` myKeys

color8, color4, myFont, background, foreground :: String

myFont = "-*-Source Code Pro-*-*-*-*-15-*-*-*-*-*-*-*"

background = "#232323"
foreground = "#CBCBCB"
color8 = "#676767"
color4 = "#307D92"

