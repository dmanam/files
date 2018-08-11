import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Paste
import XMonad.Util.Cursor
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Actions.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity (Screen))
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Core (windowset)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import XMonad.Prompt (XPConfig (..), XPPosition (Top))
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Env (envPrompt)
import XMonad.Prompt.Power (powerPrompt)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_PowerOff)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import Data.Monoid
import qualified Data.Map as M
import Data.List (isInfixOf)
import Control.Monad (void, when)
import System.Exit
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath ((</>))
import Data.Functor ((<$>))
import Data.Default
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, formatTime, defaultTimeLocale)
import System.Random (randomR, newStdGen)
import System.Environment (setEnv, lookupEnv)
import Data.Maybe (fromMaybe)

myModMask = mod4Mask
myTerminal = "urxvtc"
myFocusFollowsMouse = False
myClickJustFocuses = False
myBorderWidth = 1
homeDir = "/home/deven"
screenshotDir = homeDir </> "pictures/screenshots"
backgroundDir = homeDir </> "pictures/backgrounds"
myWorkspaces = (:[]) <$> ['α'..'κ']
myNormalBorderColor  = "#121212"
myFocusedBorderColor = "#d7d7d7"

myXPConfig = def
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

myKeys browser conf@XConfig{XMonad.modMask = modm} = M.fromList $
    [ ((modm .|.   shiftMask, xK_Return      ), spawn $ XMonad.terminal conf) -- launch a terminal
    , ((modm,                 xK_f           ), spawn browser) -- open the default browser
    , ((modm,                 xK_s           ), spawn "xinput enable 'ELAN Touchscreen'") -- enable touchscreen
    , ((modm .|.   shiftMask, xK_s           ), spawn "xinput disable 'ELAN Touchscreen'") -- disable touchscreen
    , ((modm .|.   shiftMask, xK_z           ), spawn "slock") -- lock screen
    , ((modm .|. controlMask, xK_z           ), spawn "xtrlock") -- alternate lock screen
--  , ((0,                    xK_Print       ), captureWorkspacesWhenId activePredicate moveHook horizontally)
--  , ((modm,                 xK_Print       ), captureWorkspacesWhenId defaultPredicate moveHook horizontally)
    , ((modm,                 xK_p           ), shellPrompt myXPConfig) -- command launcher
    , ((modm .|.   shiftMask, xK_n           ), envPrompt myXPConfig) -- environment variable changer
    , ((modm .|.   shiftMask, xK_c           ), kill) -- sendKey (modm .|. shiftMask) xK_c >> kill) -- close focused window
    , ((modm,                 xK_space       ), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modm .|.   shiftMask, xK_space       ), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default
    , ((modm,                 xK_n           ), refresh) -- resize viewed windows to the correct size
    , ((modm,                 xK_j           ), windows W.focusDown) -- move focus to the next window
    , ((modm,                 xK_k           ), windows W.focusUp) -- move focus to the previous window
    , ((modm,                 xK_m           ), windows W.focusMaster) -- move focus to the master window
    , ((modm,                 xK_Return      ), windows W.swapMaster) -- swap the focused window and the master window
    , ((modm .|.   shiftMask, xK_j           ), windows W.swapDown) -- swap the focused window with the next window
    , ((modm .|.   shiftMask, xK_k           ), windows W.swapUp) -- swap the focused window with the previous window
    , ((modm,                 xK_h           ), sendMessage Shrink) -- shrink the master area
    , ((modm,                 xK_l           ), sendMessage Expand) -- expand the master area
    , ((modm,                 xK_t           ), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modm,                 xK_comma       ), sendMessage (IncMasterN 1)) -- increment the number of windows in the master area
    , ((modm,                 xK_period      ), sendMessage (IncMasterN (-1))) -- deincrement the number of windows in the master area
    , ((modm,                 xK_b           ), sendMessage ToggleStruts) -- toggle the status bar gap
    , ((modm .|.   shiftMask, xK_q           ), io exitSuccess) -- quit xmonad
    , ((modm,                 xK_q           ), spawn "xmonad --restart") -- restart xmonad
    , ((modm,                 xK_v           ), pasteSelection) -- X-selection-paste buffer
    , ((modm,                 xK_g           ), withFocused toggleBorder) -- toggle showing border of current window
    , ((0,                    xF86XK_PowerOff), powerPrompt myXPConfig)
    ]
    -- mod-[1..0], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    ++ [((m .|. modm, k), windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    ++ [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings XConfig{XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)   -- grab and drag
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- grab and resize
    ]

-- layouts: use 'mod-shift-space' after updating
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
myManageHook = composeAll $
    [ manageDocks
    , className =? "qjackctl"       --> doFloat
    , className =? "Qjackctl"       --> doFloat
    ]

-- return (All True) to run default handler afterwards
myEventHook = fullscreenEventHook <+> docksEventHook

data BackgroundState = BackgroundState WorkspaceId deriving Typeable
instance ExtensionClass BackgroundState where
  initialValue = BackgroundState ""
updateBackground = do
  BackgroundState oldws <- XS.get
  newws <- W.tag . W.workspace . W.current <$> gets windowset
  when (newws /= oldws) $ do
    newBackground newws
    XS.put $ BackgroundState newws
newBackground ws = spawn $ "feh --bg-fill --no-fehbg '" <> backgroundDir </> ws <> ".png'"

myLogHook = updateBackground

myStartupHook = do
  setDefaultCursor xC_left_ptr
  io $ createDirectoryIfMissing True screenshotDir
  docksStartupHook

main = do
  browser <- fromMaybe "firefox" <$> lookupEnv "BROWSER"
  xmonad $ ewmh . pagerHints $ def
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys browser
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = myLogHook
    , startupHook        = myStartupHook
    }
