import XMonad
import Graphics.X11.ExtraTypes.XF86
import Control.Monad (void, when)
import Data.Default (Default, def)
import Data.Functor ((<$>))
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime, defaultTimeLocale, TimeLocale)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.Process (spawnProcess)
import System.Random (randomR, newStdGen)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docksStartupHook, manageDocks, ToggleStruts (ToggleStruts))
import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity (Screen))
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import XMonad.Prompt.Env (envPrompt)
import XMonad.Prompt.Power (powerPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt (XPConfig (..), XPPosition (Top))
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Util.Paste (pasteSelection)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

main = xmonad $ ewmh . pagerHints $ def
  { terminal           = "urxvtc"
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , borderWidth        = 1
  , modMask            = mod4Mask
  , workspaces         = (:[]) <$> ['α'..'κ']
  , normalBorderColor  = "#121212"
  , focusedBorderColor = "#d7d7d7"
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }

homeDir = "/home/deven"
screenshotDir = homeDir </> "pictures/screenshots"
backgroundDir = homeDir </> "pictures/backgrounds"
promptConfig = def
  { font              = "xft:DejaVuSansMono:size=10"
  , bgColor           = "#000000"
  , fgColor           = "#d7d7d7"
  , fgHLight          = "#000000"
  , bgHLight          = "#87d7ff"
  , promptBorderWidth = 0
  , position          = Top
  , height            = 30
  , searchPredicate   = isInfixOf
  }

myKeys conf@XConfig{XMonad.modMask = modm} = M.fromList $
    [ ((modm .|.   shiftMask, xK_Return               ), spawnPX (XMonad.terminal conf) []) -- launch a terminal
    , ((modm,                 xK_f                    ), spawnPX "firefox" []) -- open firefox
    , ((modm,                 xK_s                    ), spawnPX "xinput" ["enable", "'ELAN Touchscreen'"]) -- enable touchscreen
    , ((modm .|.   shiftMask, xK_s                    ), spawnPX "xinput" ["disable", "'ELAN Touchscreen'"]) -- disable touchscreen
    , ((modm .|.   shiftMask, xK_z                    ), spawnPX "slock" []) -- lock screen
    , ((modm .|. controlMask, xK_z                    ), spawnPX "xtrlock" []) -- alternate lock screen
    , ((0,                    xK_Print                ), maim) -- screenshot
    , ((modm,                 xK_Print                ), maimSel) -- screenshot selection
    , ((modm .|.   shiftMask, xK_Print                ), maimCur) -- screenshot current window
    , ((modm,                 xK_p                    ), shellPrompt promptConfig) -- command launcher
    , ((modm .|.   shiftMask, xK_n                    ), envPrompt promptConfig) -- environment variable changer
    , ((modm .|.   shiftMask, xK_c                    ), kill) -- close focused window
    , ((modm,                 xK_space                ), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modm .|.   shiftMask, xK_space                ), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default
    , ((modm,                 xK_n                    ), refresh) -- resize viewed windows to the correct size
    , ((modm,                 xK_j                    ), windows W.focusDown) -- move focus to the next window
    , ((modm,                 xK_k                    ), windows W.focusUp) -- move focus to the previous window
    , ((modm,                 xK_m                    ), windows W.focusMaster) -- move focus to the master window
    , ((modm,                 xK_Return               ), windows W.swapMaster) -- swap the focused window and the master window
    , ((modm .|.   shiftMask, xK_j                    ), windows W.swapDown) -- swap the focused window with the next window
    , ((modm .|.   shiftMask, xK_k                    ), windows W.swapUp) -- swap the focused window with the previous window
    , ((modm,                 xK_h                    ), sendMessage Shrink) -- shrink the master area
    , ((modm,                 xK_l                    ), sendMessage Expand) -- expand the master area
    , ((modm,                 xK_t                    ), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modm,                 xK_comma                ), sendMessage $ IncMasterN 1) -- increment the number of windows in the master area
    , ((modm,                 xK_period               ), sendMessage $ IncMasterN (-1)) -- decrement the number of windows in the master area
    , ((modm,                 xK_b                    ), sendMessage ToggleStruts) -- toggle the status bar gap
    , ((modm,                 xK_q                    ), spawnPX "xmonad" ["--restart"]) -- restart xmonad
    , ((modm .|.   shiftMask, xK_q                    ), io exitSuccess) -- quit xmonad
    , ((modm,                 xK_v                    ), pasteSelection) -- paste X selection buffer
    , ((modm,                 xK_g                    ), withFocused toggleBorder) -- toggle showing border of current window
    , ((0,                    xF86XK_PowerOff         ), powerPrompt promptConfig) -- show power actions
    , ((0,                    xF86XK_AudioMute        ), return ())
    , ((0,                    xF86XK_AudioLowerVolume ), return ())
    , ((0,                    xF86XK_AudioRaiseVolume ), return ())
    , ((0,                    xF86XK_MonBrightnessDown), return ())
    , ((0,                    xF86XK_MonBrightnessUp  ), return ())
    ]
    -- mod-[1..0], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    <> [((m .|. modm, k), windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] <> [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    <> [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings XConfig{XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w   >> windows W.shiftMaster) -- grab and drag
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- grab and resize
    ]

-- use 'mod-shift-space' after updating
myLayout = avoidStruts $ two ||| tall ||| wide ||| full
  where tall    = renamed [Replace "▥"] tiled
        wide    = renamed [Replace "▤"] (Mirror tiled)
        full    = renamed [Replace "□"] . noBorders $ Full
        two     = renamed [Replace "◫"] . lessBorders Screen $ TwoPane delta ratio
        tiled   = lessBorders Screen $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

-- use `xprop | grep WM_CLASS` and click to find the class
-- use 'title' to match on WM_NAME
myManageHook = composeAll $ manageDocks
  : fmap (\cl-> className =? cl --> doFloat) ["qjackctl", "Qjackctl"]

-- return (All True) to run default handler afterwards
myEventHook = fullscreenEventHook <+> docksEventHook

myLogHook = updateBackground

myStartupHook = do
  setDefaultCursor xC_left_ptr
  docksStartupHook


-- auxiliary functions

spawnPX proc args = io $ spawnProcess proc args >> return ()

instance Default TimeLocale where
  def = defaultTimeLocale

maim = maim' []
maimSel = maim' ["-s"]
maimCur = do
  Just st <- gets $ W.stack . W.workspace . W.current . windowset
  maim' ["-i", show $ W.focus st]
maim' :: [String] -> X ()
maim' args = io $ do
  utctime <- getCurrentTime
  localtz <- getCurrentTimeZone
  let localtime = utcToLocalTime localtz utctime
      filename = formatTime def"%04Y-%m-%d-%H%M%S" localtime
              <> formatTime def"%Z" localtz
              <> ".png"
  createDirectoryIfMissing True screenshotDir
  spawnProcess "maim" $ (screenshotDir </> filename) : "-u" : args
  return ()

data BackgroundState = BackgroundState WorkspaceId deriving Typeable
instance ExtensionClass BackgroundState where
  initialValue = BackgroundState ""
updateBackground = do
  BackgroundState oldws <- XS.get
  newws <- gets $ W.tag . W.workspace . W.current . windowset
  when (newws /= oldws) $ do
    newBackground newws
    XS.put $ BackgroundState newws
newBackground ws = spawnPX "feh" ["--bg-fill", "--no-fehbg", backgroundDir </> ws <> ".png"]
