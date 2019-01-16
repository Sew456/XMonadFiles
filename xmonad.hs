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
import XMonad.Util.Loggers

foreground = "#dee5ec"
background = "#111111"
myFont = "Hack:style=Regular::size=9"
leftIcon = "/home/sew/.xmonad/boxleft.xbm"
leftIcon2 = "/home/sew/.xmonad/boxleft2.xbm"
rightIcon = "/home/sew/.xmonad/boxright.xbm"
darkerForeground = "#8E9091"

--Super Metroid Color Pallette--
colorMediumYellow = "#ACA401"
colorDarkYellow = "#5A5A00"
colorLightYellow = "#EEEE00"
colorDarkRed = "#6D0101"
colorLightRed = "#DB2900"
colorDarkGreen = "#204121"
colorLighterBlack = "#191919"
colorFusionBlue = "#00B9C0"
colorWhite = "#BDBFC4"

data RectPP = RectPP
	{ bgColorRPP 		:: String,
	  fgColorRPP 		:: String,
	  leftIconRPP		:: String,
          rightIconRPP          :: String
	}

dzenBoxStyle :: RectPP -> String -> String

fusionBlueRect :: RectPP
fusionBlueRect = RectPP
		{ bgColorRPP = colorFusionBlue,
		  fgColorRPP = colorLighterBlack,
                  leftIconRPP = leftIcon2,
                  rightIconRPP = rightIcon
		}
whiteRect :: RectPP
whiteRect = RectPP
            { bgColorRPP = "#D0E0E4",
              fgColorRPP = colorLighterBlack,
              leftIconRPP = leftIcon2,
              rightIconRPP = rightIcon
            }

noWindowsBlackRect :: RectPP
noWindowsBlackRect = RectPP
            { bgColorRPP = colorLighterBlack,
              fgColorRPP = colorDarkGreen,
              leftIconRPP = leftIcon,
              rightIconRPP = rightIcon
            }
focusRect :: RectPP
focusRect = RectPP
            {
              bgColorRPP = "#131426",
              fgColorRPP = colorLightYellow,
              leftIconRPP = leftIcon,
              rightIconRPP = rightIcon
            }

withWindowsBlackRect :: RectPP
withWindowsBlackRect = RectPP
                    { bgColorRPP = colorLighterBlack,
                      fgColorRPP = foreground,
                      leftIconRPP = leftIcon,
                      rightIconRPP = rightIcon
                    }

dzenBoxStyle box t =
               "^fg(" ++ (bgColorRPP box) ++
               ")^i(" ++ (leftIconRPP box) ++
               ")^ib(1)^r(1920x14)^p(-1920)^fg(" 
                ++ (fgColorRPP box) ++ ")" ++ t ++
                "^fg(" ++ (bgColorRPP box) ++
                ")^i(" ++ (rightIconRPP box) ++
                ")^fg("++background++")^r(1920x14)^p(-1920)^fg()^ib(0)"

dzenBoxStyleLogger :: RectPP -> Logger -> Logger
dzenBoxStyleLogger box t = (fmap . fmap) (dzenBoxStyle box) t


myWorkspacesBar = "dzen2 -dock -x '0' -y '0' -w 1000 -ta 'l' -fn '"++myFont++"' -fg '"++foreground++"' -bg "++background

myPP h = def {
		ppOutput = hPutStrLn h,
		ppTitle = dzenColor colorLightYellow "" . shorten 50,
		ppCurrent = dzenBoxStyle fusionBlueRect,
		ppVisible = dzenBoxStyle withWindowsBlackRect,
		ppHidden = dzenBoxStyle withWindowsBlackRect,
		ppHiddenNoWindows = dzenBoxStyle noWindowsBlackRect,
                ppSep = "   ",
                ppOrder = \(ws:l:t:exs) -> [ws]++exs,
                ppExtras = [layoutLogger, focusLogger]
	}

--workspaces--

myWorkspaces = clickable $ ["1","2","3","4","5","6","7","8","9"]
	where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++")" ++ ws ++ "^ca()" |
				(i,ws) <- zip [1..] l,
				let n = i ] 

myLayout = name "Tall" tiled ||| name "Mirror" (Mirror tiled) ||| name "Full"  Full
	where
		name n = renamed [Replace n] 

		tiled = smartSpacing 5 $ Tall nmaster delta ratio

		nmaster = 1							-- number of windows in the master plane

		ratio = 1/2							--proportion of screen occupied by master plane

		delta = 3/100							--resize increment percentage

-- Logger Functions -- 
layoutLogger = dzenBoxStyleLogger whiteRect logLayout
focusLogger = dzenBoxStyleLogger focusRect $ shortenL 50 logTitle

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
			((0	, 0x1008ff14), spawn "mpc toggle"),
			((0	, 0x1008ff17), spawn "mpc next"),
			((0	, 0x1008ff16), spawn "mpc prev"),
			((0	, 0x1008ff15), spawn "mpc stop"),
			((mod4Mask, xK_p), spawn "dmenu_run -fn 'xos4 terminus' -sb '#800000'")
		  ]
