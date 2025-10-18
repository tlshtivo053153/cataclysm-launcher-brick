-- config/launcher.dhall の推奨される内容
-- 各ディレクトリパスは launcherRootDirectory を基準に自動生成されるため、
-- ユーザーは基本的にこのルートディレクトリのみを意識すれば良い。
let home = env:HOME ? "."
let launcherRoot = home ++ "/.cataclysm-launcher-brick"

in {
  -- 主要なディレクトリパス。必要に応じて個別に上書き可能。
  , launcherRootDirectory = launcherRoot
  , cacheDirectory = launcherRoot ++ "/cache"
  , sysRepoDirectory = launcherRoot ++ "/sys-repo"
  , userRepoDirectory = launcherRoot ++ "/user-repo"
  , sandboxDirectory = launcherRoot ++ "/sandbox"
  , backupDirectory = launcherRoot ++ "/backups"
  , downloadCacheDirectory = launcherRoot ++ "/cache/downloads"
  , soundpackCacheDirectory = launcherRoot ++ "/cache/soundpacks"
  , useSoundpackCache = True

  -- その他の設定
  , maxBackupCount = 10
  , githubApiUrl = "https://api.github.com/repos/CleverRaven/Cataclysm-DDA/releases"
  , downloadThreads = 4
  , logLevel = "Info"
  , soundpackRepos = [ "https://github.com/Kenan2000/Otopack-Mods-Updates" ]
}
