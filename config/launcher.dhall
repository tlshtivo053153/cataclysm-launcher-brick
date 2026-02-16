-- config/launcher.dhall
-- This file defines the application's configuration using a nested structure.
let home = env:HOME ? "."
let launcherRoot = home ++ "/.cataclysm-launcher-brick"

in {
  -- Directory and file paths
  , paths = {
    , launcherRoot = launcherRoot
    , cache = launcherRoot ++ "/cache"
    , sysRepo = launcherRoot ++ "/sys-repo"
    , userRepo = launcherRoot ++ "/user-repo"
    , sandbox = launcherRoot ++ "/sandbox"
    , backup = launcherRoot ++ "/backups"
    , downloadCache = launcherRoot ++ "/cache/downloads"
    , soundpackCache = launcherRoot ++ "/cache/soundpacks"
    }

  -- External API settings
  , api = {
    , githubUrl = "https://api.github.com/repos/CleverRaven/Cataclysm-DDA/releases"
    }

  -- Feature flags and behavior settings
  , features = {
    , useSoundpackCache = True
    , downloadThreads = 4
    , maxBackupCount = 10
    }

  -- Logging configuration
  , logging = {
    , level = "Info"
    }

  -- Soundpack repository URLs
  , soundpackRepos = {
    , repositories = [ "https://github.com/Kenan2000/Otopack-Mods-Updates" ]
    }

  -- Font configuration
  , fonts = {
    , availableFonts = [
        { fontName = "HackGen", fontUrl = "https://github.com/yuru7/HackGen/releases/download/v2.10.0/HackGen_v2.10.0.zip" }
      , { fontName = "PlemolJP", fontUrl = "https://github.com/yuru7/PlemolJP/releases/download/v3.0.0/PlemolJP_HS_v3.0.0.zip" }
      , { fontName = "UDEVGothic", fontUrl = "https://github.com/yuru7/udev-gothic/releases/download/v2.1.0/UDEVGothic_HS_v2.1.0.zip" }
      , { fontName = "Myrica", fontUrl = "https://github.com/tomokuni/Myrica/raw/master/product/Myrica.zip" }
      , { fontName = "MyricaM", fontUrl = "https://github.com/tomokuni/Myrica/raw/master/product/MyricaM.zip" }
      , { fontName = "Moralerspace", fontUrl = "https://github.com/yuru7/moralerspace/releases/download/v2.0.0/Moralerspace_v2.0.0.zip" }
      ]
    }
}
