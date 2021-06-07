import Control.Monad (filterM,liftM, join)
import Data.IORef
import Data.List
import Data.Monoid
import qualified Data.Map        as M
import qualified Data.Set as S
import Foreign.C.Types (CLong)
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (nextWS, prevWS)
--import XMonad.Actions.Volume
--import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.DwmStyle
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt.Shell
import XMonad.Util.Font
import XMonad.Util.Run

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "xfce4-terminal"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 6

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["0","1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#1D1D1D"
myFocusedBorderColor = "#A60038"

myTitleTheme :: Theme
myTitleTheme = def { 
      fontName            = "ProggyVector"
    , inactiveBorderColor = myNormalBorderColor
    , inactiveColor       = "#3d3d3d"
    , inactiveTextColor   = "#cdcdcd"
    , activeBorderColor   = "#A60038"
    , activeColor         = "#ff0057"
    , activeTextColor     = "#ffffff"
    , urgentBorderColor   = "#ff8600"
    , urgentTextColor     = "#000000"
    , urgentColor         = "#ffce00"
    , decoHeight          = 18
}

--volume s = spawn ("volnoti-show `amixer set Master unmute > /dev/null; amixer set Master " ++ s ++ " | grep '[0-9]*%' -o | head -n 1` " )
--mute = spawn ("`amixer set Master mute`; volnoti-show -m")
--mute = spawn ("/home/matt/perl5/perlbrew/perls/perl-5.14.4/bin/perl /home/matt/bin/mute.pl")

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- make VirtualBox ignore stray hits of the Windows key when xmonad has the active grab
    , ((0, xK_Super_L), return ())

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`/bin/bash -l -c \"dmenu_run -fn 'ProggyVector' -nb black -nf white -sb black -sf '#FF0086'\"` && eval \"exec /bin/bash -l -c '$exe'\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

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

    -- Move focus to the previous window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)

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
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm,               xK_Right),  nextWS)
    , ((modm,               xK_Left), prevWS)

    -- Restart xmonad
    , ((modm, xK_q     ), spawn "xmonad --recompile && xmonad --restart")

    , ((0, 0x1008FF18), spawn "firefox")

    , ((0, 0xff13), spawn "thunar")

    , ((0, xF86XK_AudioMute),         spawn "volume_mute.sh")
    , ((0, xF86XK_AudioRaiseVolume), spawn "volume_up.sh" )
    , ((0, xF86XK_AudioLowerVolume), spawn "volume_down.sh")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightness_down.sh 5" )
    , ((0, xF86XK_MonBrightnessUp), spawn "brightness_up.sh 5" )

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_F1     ), io (exitWith ExitSuccess))

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    -- num pad bindings..
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- Non-numeric num pad keys, sorted by number
numPadKeys = [ xK_KP_Insert, xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 0, 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             ] -- 0

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

fullFirstLayouts = noBorders Full ||| tiled ||| mirrored
  where
     mirrored = renamed [CutWordsLeft 1] $ noFrillsDeco shrinkText myTitleTheme (Mirror otiled)
     -- 
     tiled = renamed [CutWordsLeft 1] $ noFrillsDeco shrinkText myTitleTheme otiled
     -- default tiling algorithm partitions the screen into two panes
     otiled   = renamed [CutWordsLeft 1] $ spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
defaultLayouts = tiled ||| mirrored ||| noBorders Full
  where
     mirrored = renamed [CutWordsLeft 1] $ noFrillsDeco shrinkText myTitleTheme (Mirror otiled)
     -- 
     tiled = renamed [CutWordsLeft 1] $ noFrillsDeco shrinkText myTitleTheme otiled
     -- default tiling algorithm partitions the screen into two panes
     otiled   = renamed [CutWordsLeft 1] $ spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
myLayout = onWorkspaces ["2", "3", "5", "9"] fullFirstLayouts $ defaultLayouts


doTopRightFloat :: ManageHook
doTopRightFloat = ask >>= \w -> doF . W.float w . position . snd =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (0.99-w) 0.02 w h

doTopLeftFloat :: ManageHook
doTopLeftFloat = ask >>= \w -> doF . W.float w . position . snd =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 0.02 w h

doBottomRightFloat :: ManageHook
doBottomRightFloat = ask >>= \w -> doF . W.float w . position . snd =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (1-w) (1 - h) w h

doBottomLeftFloat :: ManageHook
doBottomLeftFloat = ask >>= \w -> doF . W.float w . position . snd =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 (1 - h) w h


------------------------------------------------------------------------
-- Window rules:

getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

checkAtom name value = ask >>= \w -> liftX $ do
  a <- getAtom name
  val <- getAtom value
  mbr <- getProp a w
  case mbr of
    Just [r] -> return $ elem (fromIntegral r) [val]
    _ -> return False


myFadeHook toggleFadeSet = fadeOutLogHook $ fadeIf (testCondition toggleFadeSet) 0.82
doNotFadeOutWindows = title =? "Call with " <||> className =? "xine" <||> className =? "MPlayer" <||> className =? "Firefox" <||> className =? "firefox" <||> className =? "vlc" <||> isFullscreen

testCondition :: IORef (S.Set Window) -> Query Bool
testCondition floats =
    liftM not doNotFadeOutWindows <&&> isUnfocused
    <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s | w `S.member` s = S.delete w s
                  | otherwise = S.insert w s

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll . concat $
    [
      [ className =? "trayer"          --> doIgnore ]
    , [ className =? "zoom" --> doF (W.shift "5") <+> doFullFloat ]
    , [ appName =? "zoom" --> doF (W.shift "5") <+> doFullFloat ]
    , [ className =? "Wine"           --> doF (W.shift "5") ]
    , [ className =? "explorer.exe"    --> doF (W.shift "5") ]
    , [ title =? "spice display 0:0"         --> doF (W.shift "3") ]
    , [ className =? "Mail"           --> doF (W.shift "4") ]
    , [ title =? "Sid Meier's Civilization V"  --> doF (W.shift "5") ]
    , [ className =? "snes9x-gtk"     --> doIgnore ]
    , [ title =? "Sup"  --> doF (W.shift "4") ]
    , [ title =? "Borderlands 2"  --> doF (W.shift "5") ]
    , [ className =? "MPlayer" --> doF (W.shift "0") ]
    , [ title =? "Steam Guard - Computer Authorization Required" --> doCenterFloat ]
    , [ title =? "File Operation Progress" --> doCenterFloat ]
    , [ fmap ( c `isPrefixOf`) title --> doCenterFloat | c <- myMatchPrefixIgnoresT ]
    , [ fmap ( c `isPrefixOf`) title --> doFloat | c <- myMatchPrefixFloatsT ]
    , [ (className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role)  --> doCenterFloat ]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ checkDialog --> doCenterFloat ]
-- don't draw a border on fullscreen windows, or vms
    , [ className =? "VirtualBox Machine" <||> className =? "vlc" <||> isFullscreen --> doFullFloat <+> hasBorder False ]
    ]
  where
    copyToWss ids win = map (copyWindow win) ids

    role = stringProperty "WM_WINDOW_ROLE"

    checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

    myMatchPrefixFloatsT = ["win"]

    myMatchPrefixIgnoresT = ["Android"]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook
  <+> fullscreenEventHook
  <+> hintsEventHook

-- myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook h = do
  fadeInactiveLogHook 0.82
  dynamicLogWithPP $ xmobarPP
    { ppSep = "<fc=#A60038>|</fc>"
    , ppCurrent = xmobarColor "#FF0086" "" . wrap "[" "]"
    , ppUrgent = xmobarColor "yellow" ""
    , ppTitle = xmobarColor "white" "" . shorten 150
    , ppOutput = hPutStrLn h }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  ewmhDesktopsStartup
  spawn "/home/matt/bin/xmonad_startup.pl"
  spawn "/usr/bin/notify-send 'xmonad' 'xmonad started or restarted.'"
  setWMName "XMONAD"
  return()

myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = urgencyConfig {
  suppressWhen = Focused
}

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
    xmproc <- spawnPipe "/home/matt/.local/bin/xmobar /home/matt/.xmonad/.xmobarrc"
    toggleFadeSet <- newIORef S.empty
    xmonad $ docks $ ewmh $ withUrgencyHookC NoUrgencyHook myUrgencyConfig defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
      --  numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = avoidStrutsOn [U] $ smartBorders $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            = myLogHook xmproc >> myFadeHook toggleFadeSet
    }

