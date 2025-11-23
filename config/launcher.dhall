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
}
