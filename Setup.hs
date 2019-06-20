import Distribution.MacOSX
import Distribution.Simple

--main = defaultMain

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postBuild = appBundleBuildHook guiApps
  }

guiApps :: [MacApp]
guiApps =
  [ MacApp
    { appName = "Peoplemon"
    , appIcon = Just "peoplemon.icns"
    , appPlist = Nothing -- Build a default Info.plist for the icon.
    , resources = [] -- No other resources.
    , otherBins = []
    , appDeps = ChaseWith (defaultExclusions ++ ["/usr/lib/"]) --DoNotChase -- Try changing to ChaseWithDefaults
    }
  ]
