import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ratio ((%))
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.RotSlaves
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Dishes
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.Themes
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W

myMod = mod4Mask -- windows key
myTerminal = "/usr/bin/sakura -e /home/edd/bin/grab-vars-and-screen"

myWorkSpaces = ["logs", "mail", "main", "web", "x", "chat"]

myTheme = defaultTheme
	{ activeColor         = blue
	, inactiveColor       = grey
	, activeBorderColor   = blue
	, inactiveBorderColor = grey
	, activeTextColor     = "white"
	, inactiveTextColor   = "black"
	, decoHeight          = 27
	}
	where
		blue = "#ccc"
		grey = "#3c3c3c"

myXPConfig = defaultXPConfig
	{ fgColor  = "#ccc"
	, bgColor  = "black"
	, promptBorderWidth = 0
	, position = Bottom
	, height   = 27
	, font     = "xft:Ubuntu Mono-11"
	}

myLayout = smartBorders $ toggleLayouts Full perWS
	where
		-- Per workspace layout selection.
		perWS =
			onWorkspace "logs" (noTitles $ myLogs dishFirst) $
			onWorkspace "mail" (noTitles myMail) $
			onWorkspace "web"  (noTitles $ (mySplit ||| myWide)) $
			onWorkspace "chat" (noTitles $ myChat Grid ||| dishFirst) $
			                   (noTitles $ codeFirst)

		withTitles l = noFrillsDeco shrinkText myTheme $ desktopLayoutModifiers l

		-- Modifies a layout to be desktop friendly, but with no title bars
		-- and avoid the panel.
		noTitles l = desktopLayoutModifiers l

		-- Each of these allows toggling through a set of layouts
		-- in the same logical order, but from a different starting
		-- point.
		codeFirst = myCode ||| myWide ||| myGrid ||| myDish
		dishFirst = myDish ||| myCode ||| myWide ||| myGrid

		-- This is a tall-like layout with magnification.
		-- The master window is fixed at 80 columns wide, making
		-- this good for coding. Limited to 3 visible windows at
		-- a time to ensure all are a good size.
		myCode = limitWindows 3 $ magnifiercz' 1.4 $
			FixedColumn 1 20 80 10

		-- Stack with one large master window.
		-- It's easy to overflow a stack to the point that windows
		-- are too small, so only show first 5.
		myDish = limitWindows 5 $ Dishes nmaster ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- proportion of screen occupied by other panes
				ratio = 1/5

		-- Wide layout with subwindows at the bottom.
		myWide = Mirror $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 80/100

		myMail = Mirror $ Tall nmaster delta ratio
			where 
				nmaster = 1
				delta   = 3/100
				ratio   = 90/100

		-- Split screen, optimized for web browsing.
		mySplit = magnifiercz' 1.4 $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 60/100

		-- Standard grid.
		myGrid = Grid

		-- The chat workspace has a roster on the right.
		myChat base = mirror base $ withIM size roster
			where
				-- Ratio of screen roster will occupy
				size = 5%23
				-- Match roster window
				roster = Title "Contact List"

		-- The logs workspace has space for procmeter.
		myLogs base = mirror base $ withIM procmeterSize procmeter
			where
				-- Ratio of screen procmeter will occupy
				procmeterSize = 1%7
				-- Match procmeter
				procmeter = ClassName "ProcMeter3"

		-- For reading books, I typically want borders on 
		-- the margin of the screen.
		--myBook = ThreeColMid nmaster delta ratio
			--where
				---- default number of windows in the master pane
				--nmaster = 1
				-- Percent of screen to increment by when resizing panes
				--delta   = 3/100
				-- proportion of screen occupied by master pane
				--ratio   = 2/3

		-- Applies a layout mirrored.
		mirror base a = reflectHoriz $ a $ reflectHoriz base
myKeys =
	[ ("M-x", spawn myTerminal)
	, ("M-S-q", spawn "gnome-session-quit")
	, ("M-c", kill)
	, ("M-S-h", sendMessage Shrink)
	, ("M-S-l", sendMessage Expand)
	, ("M-h", prevWS)
	, ("M-l", nextWS)
	, ("M-<Left>", prevWS)
	, ("M-<Right>", nextWS)
	, ("M-a", myToggle)
	, ("M-z", shellPrompt myXPConfig)
	-- xfce terminal does not support params the scratchpad needs
	, ("M-s", spawn "scrot")
	, ("M-<Space>", sendMessage $ NextLayout)
	, ("M-d", sendMessage $ ToggleLayout)
	, ("<XF86Launch1>", spawn "/home/edd/bin/keys.sh")
	, ("<XF86Launch2>", spawn "iceweasel")
	, ("<XF86Launch3>", spawn "gpodder")
	, ("C-M-<Space>", spawn "/home/edd/bin/music toggle-playing")
	, ("C-M-h", spawn "/home/edd/bin/music restart-or-previous")
	, ("C-M-j", spawn "/home/edd/bin/music volume '-10'")
	, ("C-M-k", spawn "/home/edd/bin/music volume '+10'")
	, ("C-M-l", spawn "/home/edd/bin/music next")
	]

myManageHook = composeAll
	-- comes first to partially override default gimp floating behavior
	[ gimp "toolbox" --> nofloat
	, gimp "image-window" --> nofloat
	, manageHook gnomeConfig
	, doF avoidMaster
	, scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)
	, resource =? "floatterm" --> doFloat
	-- workaround for <http://code.google.com/p/xmonad/issues/detail?id=228>
	, composeOne [ isFullscreen -?> doFullFloat ]
	, terminal "tmon" --> doShift "logs"
	, terminal "tmutt" --> doShift "mail"
	, (className =? "Iceweasel") --> doShift "web"
	]
	where
		terminal name = className =? "Sakura" <&&> fmap (name ==) title
		gimp win = className =? "Gimp" <&&> fmap (win `isSuffixOf`) role
		role = stringProperty "WM_WINDOW_ROLE"
		nofloat = ask >>= doF . W.sink

myLogHook h = dynamicLogWithPP $ dzenPP
	{ ppOutput = hPutStrLn h
	, ppCurrent = dzenColor "black" blue . pad
	, ppHidden = dzenColor lightgray "" . pad . take 1
	, ppHiddenNoWindows = dzenColor darkgray "" . pad . take 1
	, ppLayout = blank
	, ppTitle = dzenColor blue "" . pad
	, ppUrgent = dzenColor red "" . pad
	}
	where
		blue = activeColor myTheme
		lightgray = "#ccc"
		darkgray = "#3c3c3c"
		red = "#f66"
		blank s = ""

-- Modified to only operate on floating windows, since I seem to do this by
-- accident to non-floating.
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
	-- mod-button1, Move by dragging
	[ ((modMask, button1), (\w -> do focus w; ifFloating w mouseMoveWindow))
	-- mod-button2, Raise the window to the top of the stack
	, ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
	-- mod-button3, Resize by dragging
	, ((modMask, button3), (\w -> do focus w; ifFloating w mouseResizeWindow))
	]
	where
		ifFloating w f = withWindowSet $ \ws ->
			when (M.member w $ W.floating ws) (f w)

myConfig = ewmh gnomeConfig
	{ manageHook = myManageHook
	, layoutHook = myLayout
	, modMask = myMod
	, workspaces = myWorkSpaces
	, mouseBindings = myMouseBindings
	, terminal = myTerminal
	, borderWidth = 2
	, normalBorderColor  = inactiveBorderColor myTheme
	, focusedBorderColor = activeBorderColor myTheme
	} `additionalKeysP` myKeys

main = do
	xmproc <- spawnPipe "/usr/local/bin/dzen2 -ta l -w 420 -h 29 -bg black -x 604 -fn 'Ubuntu Mono-10' -e onstart=lower"
	conky <- spawnPipe "/usr/bin/conky | /usr/local/bin/dzen2 -ta l -w 420 -h 29 -bg black -x 0 -fn 'Ubuntu Mono-10'" -- -l 18
	xmonad $ withUrgencyHook NoUrgencyHook $ myConfig
		{ logHook = myLogHook xmproc
		}


-- Avoid the master window, but otherwise manage new windows normally.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c ->
	case c of
		W.Stack t [] (r:rs) -> W.Stack t [r] rs
		_ -> c

-- A version of toggleWS that ignores the scratchPad workspaces.
myToggle = windows $ W.view =<< W.tag . head . filter (notSP . W.tag) . W.hidden
	where
		notSP x = x /= "NSP" && x /= "SP"


-- vi: set ts=4 sw=4 :
