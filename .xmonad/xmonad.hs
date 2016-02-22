import           Data.Monoid              (All)
import           System.Exit
import           System.IO                (Handle)
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Spacing
import           XMonad.Util.Run

import qualified Data.Map                 as M
import qualified XMonad.StackSet          as W

myTerminal :: String
myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth   = 2
myModMask :: KeyMask
myModMask       = mod4Mask

myWorkspaces :: [String]
myWorkspaces = [devel, code, web, music, media, mail, misc]
               where music = "   ^i(/home/creek/.xmonad/icons/Music.xbm)"
                     web   = "   ^i(/home/creek/.xmonad/icons/www.xbm)"
                     code  = "   ^i(/home/creek/.xmonad/icons/code.xbm)"
                     devel = "   ^i(/home/creek/.xmonad/icons/Devel.xbm)"
                     media = "   ^i(/home/creek/.xmonad/icons/media.xbm)"
                     mail  = "   ^i(/home/creek/.xmonad/icons/mail.xbm)"
                     misc  = "   ^i(/home/creek/.xmonad/icons/pacman.xbm)"
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor :: String
myFocusedBorderColor = "#00ff00"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig{XMonad.modMask = modm} = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch chromium
    , ((modm,               xK_c     ), spawn "chromium")

    -- launch emacs
    , ((modm,               xK_x     ), spawn "emacsclient -c -a \"\"")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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

myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 3/5
    delta   = 3/100

myManageHook :: ManageHook
myManageHook = composeAll []

myEventHook :: Event -> X All
myEventHook = mempty

myLogHook :: Handle -> X ()
myLogHook dzproc =
  dynamicLogWithPP $ dzenPP {
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

bar1Width = "800"
bar2Width = "566"

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
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = smartSpacing 5 $ avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> docksEventHook,
        logHook            = myLogHook dzproc,
        startupHook        = myStartupHook
    }

color8, color4, myFont, background, foreground :: String

myFont = "-*-Source Code Pro-*-*-*-*-15-*-*-*-*-*-*-*"

background = "#232323"
foreground = "#CBCBCB"
color8 = "#676767"
color4 = "#307D92"

