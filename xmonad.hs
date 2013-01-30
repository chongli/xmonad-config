--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import Control.Monad (filterM)
import Data.Char (ord)

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.Renamed


import XMonad.Util.Run (spawnPipe)

import Control.Monad (liftM2)
import Data.Monoid (mconcat)
import System.Exit
import System.IO (hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = names ++ map show [length names..9]
  where
    names = ["media", "web", "steam", "mpd", "gimp"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#121212"
myFocusedBorderColor = "#303030"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- mpc controls
    , ((0,                  xK_F9    ), spawn "mpc prev")
    , ((0,                  xK_F10   ), spawn "mpc toggle")
    , ((0,                  xK_F11   ), spawn "mpc next")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_Tab   ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_Tab   ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )

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

    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) (xK_grave : [xK_1 .. xK_9])
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
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

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
myLayout = onWorkspace "steam" steamL
         $ onWorkspace "media" media
           basic
  where
    --gimpLayout = combineTwoP (TwoPane 0.04 0.82) (tabbedLayout) (Full) (Not (Role "gimp-toolbox"))
    basic   = smartBorders . avoidStruts $ (tiled ||| Mirror tiled ||| Full)
    media   = smartBorders $ Full
    steamL  = noBorders    . avoidStruts . renamed [Replace "Steam"] $ steam
    steam   = combineTwoP mirror twoP ims (Or (Title "Steam") (ClassName "Mumble"))
    ims     = combineTwoP twoP friends pidgin (And (ClassName "Steam") (Not (Title "Steam")))
    friends = combineTwoP twoP Full Full (And (ClassName "Steam") (Title "Friends"))
    pidgin  = combineTwoP twoP Full Full (And (ClassName "Pidgin") (Not (Title "Buddy List")))
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    twoP    = TwoPane delta ratio
    mirror  = Mirror twoP

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

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
myManageHook =
    composeOne
    [ isFullscreen -?> doFull "media"
    , isDialog     -?> doCenterFloat
    ]
    <+> composeAll
    [ className =? "Mumble"                  --> doShift "steam"
    , className =? "Steam"                   --> doShift "steam"
    , title     =? "Wine System Tray"        --> doHideIgnore
    , title     =? "Emacs TEXTAREA"          --> doCenterFloat
    , className =? "Pidgin"                  --> doShift "steam"
    , appName   =? "brogue"                  --> doFull  "media"
    , title     =? "Dungeons of Dredmor"     --> doFull  "media"
    , className =? "Vlc"                     --> doFull  "media"
    , className =? "t-engine"                --> doFull  "media"
    , appName   =? "spelunky.exe"            --> doFull  "media"
    , appName   =? "AIWar.exe"               --> doFull  "media"
    , appName   =? "SWT"                     --> doFull  "media"
    , className =? "FTL"                     --> doFull  "media"
    , className =? "Dwarf_Fortress"          --> doFull  "media"
    , command   =? "urxvt-ezsh-cncmpcpp"     --> doShift "mpd"
    , className =? "dwarftherapist"          --> doShift "gimp"
    , className =? "Gimp"                    --> doShift "gimp"
    , title     =? "Task Manager - Chromium" --> doFloat
    , role      =? "pop-up"                  --> doFloat
    ]
    <+> manageDocks
    where
        printable = return . (> 32) . ord
        command   = stringProperty "WM_COMMAND" >>= filterM printable
        role      = stringProperty "WM_WINDOW_ROLE"
        shiftV    = doF . liftM2 (.) W.greedyView W.shift
        doSink    = ask >>= \w -> doF (W.sink w)
        doFull s  = doSink <+> shiftV s

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mconcat [docksEventHook,fullscreenEventHook]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X evet.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook h = dynamicLogWithPP myPP { ppOutput = hPutStrLn h }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Status bar

-- Command to start a statusbar when XMonad starts
--myStatusBar = "dzen2 -e '' -w 1400 -ta l -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*' -bg black -fg #d3d7cf "
myStatusBar = "xmobar"

myPP = defaultPP { ppCurrent = xmobarColor green "" . wrap "[" "]"
                 , ppHidden  = xmobarColor pink ""
                 , ppVisible = wrap "(" ")"
                 , ppTitle   = xmobarColor orange "" . shorten 80
                 , ppLayout  = xmobarColor blue ""
                 , ppUrgent  = xmobarColor "red" "yellow"
                 }
    where
    green  = "#afd787"
    pink   = "#d7afd7"
    orange = "#d7af87"
    blue   = "#87afd7"
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
    mapM_ spawn [ "steam"
                , "mumble"
                , "pidgin"
                , "nc-mpc"
                ]
    h <- spawnPipe myStatusBar
    xmonad $ defaults { logHook = myLogHook h }

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig
    {   terminal           = myTerminal
    ,   focusFollowsMouse  = myFocusFollowsMouse
    ,   borderWidth        = myBorderWidth
    ,   modMask            = myModMask
    ,   workspaces         = myWorkspaces
    ,   normalBorderColor  = myNormalBorderColor
    ,   focusedBorderColor = myFocusedBorderColor
    ,   keys               = myKeys
    ,   mouseBindings      = myMouseBindings
    ,   layoutHook         = myLayout
    ,   manageHook         = myManageHook
    ,   handleEventHook    = myEventHook
    ,   startupHook        = myStartupHook
    }
