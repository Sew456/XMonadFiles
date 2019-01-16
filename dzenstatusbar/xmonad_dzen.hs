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

foreground = "#FFFFFF"
background = "#000000"
myFont = "Hack:style=Regular::size=9"


--Super Metroid Color Pallette--
colorMediumYellow = "#ACA401"
colorDarkYellow = "#5A5A00"
colorLightYellow = "#EEEE00"
colorDarkRed = "#6D0101"
colorLightRed = "#DB2900"
colorDarkGreen = "#204121"

data RectPP = RectPP
	{ bgColorRPP 		:: String,
	  fgColorRPP 		:: String,
	  rectLengthRPP 	:: Int,
	  rectHeightRPP		:: Int
	}

dzenBoxSyle :: RectPP -> String

mediumYellowRect :: RectPP
mediumYellowRect = RectPP
		{ bgColor = colorMediumYellow,
		  fgColor = foreground,
		}

dzenBoxStyle Box =
		"^fg(" ++ (fgColor Box) ++
		")^ib(1)^r(50x100)^p(-100)"

myWorkspacesBar = "dzen2 -dock -x '0' -y '0' -w 600 -ta 'l' -fn '"++myFont++"' -fg '"++foreground++"' -bg "++background

myPP h = def {
		ppOutput = hPutStrLn h,
		ppTitle = dzenColor "#658a37" "" . shorten 50,
		ppCurrent = dzenBoxStyle mediumYellowRect . wrap "" ""
	}

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

main = do
	dzenLeftBar <- spawnPipe myWorkspacesBar
	xmonad $ docks $ def
		{
			manageHook	= manageDocks <+> manageHook def,
			layoutHook	= avoidStruts $ myLayout,
			terminal	= "termite",		--default terminal is termite (currently installed)
			workspaces	= myWorkspaces,
			modMask		= mod4Mask,		--rebind mod key to the windows key
			borderWidth	= 3,			--window decoration border
			focusedBorderColor = "#800000",
			logHook		= dynamicLogWithPP $ myPP dzenLeftBar
		}`additionalKeys`
		  [
			((0	, 0x1008FF13), spawn "/home/sew/pulse-volume.sh increase"),
			((0	, 0x1008FF11), spawn "/home/sew/pulse-volume.sh decrease"),
			((mod4Mask, xK_p), spawn "dmenu_run -fn 'xos4 terminus' -sb '#800000'")
		  ]
