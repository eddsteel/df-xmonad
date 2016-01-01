{-# LANGUAGE DeriveDataTypeable #-}
import XMonad

import Data.List(intersperse)
import DBus.Client
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.WindowGo (runOrRaise, className, raiseEditor, raiseBrowser, (<||>))
import XMonad.Util.Run
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad

--import System.Taffybar.Hooks.PagerHints (pagerHints)
--import System.Taffybar.XMonadLog (dbusLog)

import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Char (toUpper)


titleCase s = (toUpper . head) s : tail s

-- TODO sleep inhibit through xset s off/ turn off redshift

-- TODO: make this a raise/run *and put in master pane*
showMe :: String -> X ()
showMe s = runOrRaise s (className =? titleCase s <||> className =? s)

-- TODO: ensure this is the best way to run one-shot commands
run :: String -> X ()
run = spawn

data SShot = All | Sel
screenshot t = run $ "scrot " ++ s t ++ "'%F-%s.png' -e 'mv $f ~/Desktop'"
  where s All = ""
        s Sel = "-s "


data Emacs = Edit String | SudoEdit String | Execute String
emacs :: Emacs -> X ()
emacs o = do
  showMe "emacs"
  (spawn . concat) $ "emacsclient " : sfx o
    where sfx (Edit s) = ["-n ", s]
          sfx (SudoEdit s) = ["-n ", "/sudo::", s]
          sfx (Execute s) = ["-e ", s]

pamixer :: String
pamixer = "$(pactl list short sinks | cut -f1 | tail -n1)"

-- | Produce a pulse audio command
--
-- >>> pa ["set-sink-mute", "toggle"]
-- "pactl set-sink-mute $(pactl list short sinks | cut -f1 | tail -n1) toggle"
--
pa :: [String] -> String
pa (command:commands) = "pactl " ++ command ++ " " ++ pamixer ++ " " ++ ((concat . intersperse " ") commands)

layout = id
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT)
         $ avoidStruts(noBorders tiled ||| noBorders (Mirror tiled) ||| noBorders even ||| noBorders (Mirror even))
  where
    tiled = Tall 1 (3/100) (4/7) -- emacs at 3/7 width ~= 80 columns
    even = Tall 1 (3/100) (1/2)

extraKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask .|. shiftMask, xK_e), showMe "emacs")                  -- %! Show emacs
  , ((modMask .|. shiftMask, xK_w), showMe "chromium-browser")       -- %! Show chromium

  , ((modMask,               xK_c), kill)                            -- %! Close current window
  , ((modMask .|. shiftMask, xK_f), sendMessage (Toggle FULL))

  -- F keys, in order
  , ((0,               0x1008FF12), (run . pa) ["set-sink-mute", "toggle"])  -- %! Mute/Unmute sound
  , ((controlMask,     0x1008FF12), run "pavucontrol")
  , ((0,               0x1008FF11), (run . pa) ["set-sink-volume", "-10%"])  -- %! Decrease sound volume
  , ((controlMask,     0x1008FF11), run "pavucontrol")
  , ((0,               0x1008FF13), (run . pa) ["set-sink-volume", "+10%"])  -- %! Increase sound volume
  , ((controlMask,     0x1008FF13), run "pavucontrol")
  , ((0,               0x1008FFB2), run "amixer sset Mic toggle")    -- %! Mute/Unmute mic

  , ((0,               0x1008FF03), run "xbacklight -15")            -- %! Decrease brightness
  , ((0,               0x1008FF02), run "xbbacklight +15")           -- %! Incrase brightness
  -- display toggle
  , ((0,               0x1008FF59), run "tootch.sh toggle")          -- %! Toggle bluetooth
    -- wireless toggle - this seems to be hardware-level

  , ((0,               0x1008FF81), emacs (SudoEdit "/etc/nixos/configuration.nix"))
  , ((shiftMask,       0x1008FF81), emacs (Edit "~/.xmonad/xmonad.hs"))
  , ((controlMask .|. shiftMask, 0x1008FF81), emacs (Edit "~/.emacs.d/init.el"))
  , ((0,               0x1008FF1B), emacs (Edit "~/.org/home.org"))
  -- search
  -- window list
  -- exposé

    , ((modMask,               xK_space), shellPrompt promptConfig)            -- %! shell prompt
    , ((modMask .|. shiftMask, xK_space), xmonadPrompt promptConfig)           -- %! xmonad prompt
    , ((modMask,                   xK_i), sendMessage NextLayout)              -- %! Rotate through available layouts
    , ((modMask .|. shiftMask,     xK_i), setLayout $ XMonad.layoutHook conf)  -- %! Reset layout
    -- TODO brainzo prompt
    -- TODO google prompt
  , ((0,                xK_Print), screenshot All)                   -- %! Screenshot
  , ((shiftMask,        xK_Print), screenshot Sel)                   -- %! Screenshot window or rectangle
  ]

promptConfig = defaultXPConfig
               { fgColor = "#ccc"
               , bgColor = "#222"
               , promptBorderWidth = 0
               , position = Bottom
               , height = 30
               , font = "xft:Droid Sans Mono-12"
               }

fading = composeAll [isUnfocused                   --> transparency 0.15
                    , className =? "Google-chrome" --> opaque
                    , className =? "vlc" --> opaque
                    , fmap not isUnfocused         --> opaque
                    ]

main = do
  client <- connectSession
  let pp = defaultPP
  xmonad $
    ewmh $
--    pagerHints $
    defaultConfig
    { modMask = mod4Mask
    , normalBorderColor  = "#777777"
    , focusedBorderColor = "#FF6040"
    , keys = (\x -> extraKeys x `M.union` keys defaultConfig x)
    , layoutHook = layout
    , manageHook = manageDocks
    , logHook = fadeWindowsLogHook fading
    , handleEventHook = fadeWindowsEventHook
    }
