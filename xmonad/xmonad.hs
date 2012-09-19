-- import Control.OldException (catchDyn,try)
-- import DBus
-- import DBus.Connection
-- import DBus.Message
import System.IO
import XMonad hiding (Tall)
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.HintedTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Replace
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)


-- Was used for the gnome-panel applet
-- main = withConnection Session $ \ dbus -> do
--     getWellKnownName dbus
--     xmonad $ xfceConfig

main = do
    replace
    xmonad $ xfceConfig
        { borderWidth = 2
        , normalBorderColor = "#073642"
        , focusedBorderColor = "#268bd2"
        , terminal = "xfce4-terminal"
        , layoutHook = smartBorders (desktopLayoutModifiers myLayout)
        -- , logHook = dynamicLogWithPP (myPrettyPrinter dbus)
        , manageHook = manageHook xfceConfig <+> myManageHook
        -- , workspaces = ["1. web", "2. irc", "3. code", "4", "5", "6", "7", "8. music", "9"]
        , workspaces = [show i | i <- [1..9]]
        -- map show [1..9] would also work
        }
        `additionalKeysP`
        -- toggles Full layout
        [ ("M-C-<Space>", sendMessage ToggleLayout)
        , ("M-p", spawn "gmrun")
        ]


myLayout = toggleLayouts full (hintedTile Tall ||| hintedTile Wide ||| Circle)
  where
    -- Partial application of this, completes with Tall and Wide
    hintedTile = HintedTile numMaster delta golden Center
    full = layoutHintsToCenter Full
    numMaster = 1
    golden = toRational (2/(1+sqrt(5)::Double)) -- golden ratio
    delta = 2.5/100


myManageHook = composeAll
             [ className =? "Gimp" --> doFloat
             -- , className =? "Gvim" --> doShift "3. code"
             -- , className =? "Google-chrome" --> doShift "1. web"
             , className =? "MPlayer" --> doFloat
             , className =? "Gmrun" --> doFloat
             , title =? "Save File" --> doFloat
             , title =? "Edit Bookmark" --> doFloat
             , insertPosition End Newer
             ]


-- myPrettyPrinter :: Connection -> PP
-- myPrettyPrinter dbus = defaultPP
--     { ppOutput = outputThroughDBus dbus
--     , ppCurrent = pangoColor "#d33682" . pad . pangoSanitize
--     , ppHidden = pangoColor "#586e75" . pad . pangoSanitize
--     , ppLayout = pangoColor "#6c71c4" . pad . pangoSanitize
--     , ppTitle = pangoColor "#268bd2" . pad . shorten 50 . pangoSanitize
--     --, ppVisible = pangoColor "#663366" . wrap "(" ")" . pangoSanitize
--     , ppUrgent = pangoColor "#e35d5d"
--     , ppSep = "<span foreground=\"#555753\"> :: </span>"
--     }
--     -- jellybeans colors
--     -- , ppCurrent = pangoColor "#8f92cf" . pad . pangoSanitize
--     -- , ppHidden = pangoColor "#6b6c8c" . pad . pangoSanitize
--     -- , ppLayout = pangoColor "#53728f" . pad . pangoSanitize
--     -- , ppTitle = pangoColor "#dba160" . pad . shorten 50 . pangoSanitize
-- 
-- 
-- getWellKnownName :: Connection -> IO ()
-- getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) -> getWellKnownName dbus)
--   where
--     tryGetName = do
--         nameReq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
--         addArgs nameReq [String "org.xmonad.Log", Word32 5]
--         sendWithReplyAndBlock dbus nameReq 0
--         return()
-- 
-- 
-- outputThroughDBus :: Connection -> String -> IO ()
-- outputThroughDBus dbus str = do
--     let str' = "<span font=\"Molengo 10\">" ++ str ++ "</span>"
--     msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
--     addArgs msg [String str']
--     send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
--     return()
-- 
-- 
-- pangoColor :: String -> String -> String
-- pangoColor fg = wrap left right
--   where
--     left  = "<span foreground=\"" ++ fg ++ "\">"
--     right = "</span>"
-- 
-- 
-- pangoSanitize :: String -> String
-- pangoSanitize = foldr sanitize ""
--   where
--     sanitize '>'  acc = "&gt;" ++ acc
--     sanitize '<'  acc = "&lt;" ++ acc
--     sanitize '\"' acc = "&quot;" ++ acc
--     sanitize '&'  acc = "&amp;" ++ acc
--     sanitize x    acc = x:acc
