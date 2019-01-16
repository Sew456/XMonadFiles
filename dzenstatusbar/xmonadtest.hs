import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import Data.List
import XMonad.Util.EZConfig(additionalKeys)

--variables--
foreground = "#FFFFFF"
background = "#000000"
myFont = ""


--workspaces--

myWorkspaces = ["I","II","III","IV","V","VI","VII","VIII","IX"] 

myLayout = name "Tall" tiled ||| name "Mirror Tall" (Mirror tiled) ||| name "Full"  Full
	where
		name n = renamed [Replace n] 

		tiled = smartSpacing 5 $ Tall nmaster delta ratio

		nmaster = 1							-- number of windows in the master plane

		ratio = 1/2							--proportion of screen occupied by master plane

		delta = 3/100							--resize increment percentage

--main function--
myWorkspacesBar = "dzen -x '0' -y '0' -w 600 -fg '" ++foreground++ "' -bg '" ++background++ "'"


main = do
	dzenLeftBar <- spawnPipe myWorkspacesBar
	--dzenRightBar <- spawnPipe myStatusBar
	xmonad $ docks $ defaultConfig
		{
			manageHook	= manageDocks <+> manageHook defaultConfig,
			layoutHook	= avoidStruts $ myLayout,
			terminal	= "termite",		--default terminal is termite (currently installed)
			workspaces	= myWorkspaces,
			modMask		= mod4Mask,		--rebind mod key to the windows key
			borderWidth	= 3,			--window decoration border
			focusedBorderColor = "#800000",
			logHook		= dynamicLogWithPP dzenLeftBar
						{
							ppOutput = hPutStrLn h,
							ppTitle = xmobarColor "#658a37" "" . shorten 50,
							ppCurrent = xmobarColor "#658a37" "" . wrap "" ""
						}
		}`additionalKeys`
