module Types.Event (
    UIEvent(..)
  , DownloadInfo(..)
  , DownloadProgress(..)
) where

import qualified Data.Text as T
import Data.Time (UTCTime)
import Types.Domain
import Types.Font (FontInfo, InstalledFont)
import Types.Error (ManagerError)

-- | ダウンロード情報
data DownloadInfo = DownloadInfo
    { diName :: T.Text          -- ^ 表示名（ゲームバージョン、サウンドパック名等）
    , diFileName :: T.Text      -- ^ ファイル名
    , diTotalBytes :: Int       -- ^ 総バイト数（Content-Length）
    , diStartTime :: UTCTime    -- ^ 開始時刻
    } deriving (Eq, Show)

-- | ダウンロード進捗
data DownloadProgress = DownloadProgress
    { dpFileName :: T.Text      -- ^ ファイル名
    , dpDownloaded :: Int       -- ^ ダウンロード済みバイト数
    , dpTotalBytes :: Int       -- ^ 総バイト数
    } deriving (Eq, Show)

data UIEvent
  = LogMessage T.Text
  | LogEvent T.Text
  | ErrorEvent T.Text
  | CacheHit T.Text
  | InstallFinished (Either ManagerError String)
  | GameUninstalled (Either ManagerError InstalledVersion)
  | ProfileCreated (Either ManagerError SandboxProfile)
  | ProfileDeleted (Either ManagerError SandboxProfile)
  | BackupCreated (Either ManagerError ())
  | BackupRestored (Either ManagerError ())
  | BackupDeleted (Either ManagerError BackupInfo)
  | BackupsListed (Either ManagerError [BackupInfo])
  | ModInstallFinished (Either ModHandlerError ModInfo)
  | ModEnableFinished (Either ModHandlerError ())
  | ModDisableFinished (Either ModHandlerError ())
  | ModUninstalled (Either ModHandlerError ModInfo)
  | AvailableModsListed ([AvailableMod], [ModInfo])
  | ActiveModsListed [ModInfo]
  | FetchSoundpacks
  | InstallSoundpack SoundpackInfo
  | UninstallSoundpack InstalledSoundpack
  | SoundpackInstallFinished (Either ManagerError InstalledSoundpack)
  | SoundpackUninstallFinished (Either ManagerError InstalledSoundpack)
  | InstalledSoundpacksListed [InstalledSoundpack]
  | ProfileSelectionChanged
  | InstallFont FontInfo
  | FontInstallFinished (Either ManagerError InstalledFont)
  | ActivateFont SandboxProfile InstalledFont
  | FontActivationFinished (Either ManagerError ())
  | UninstallFont InstalledFont
  | FontUninstalled (Either ManagerError InstalledFont)
  | InstalledFontsListed [InstalledFont]
  | ForceRefreshVersions
  | VersionsRefreshed (Either String [GameVersion])
  -- Download progress events
  | DownloadStarted DownloadInfo
  | DownloadProgressUpdate DownloadProgress
  | DownloadFinished T.Text
  | DownloadFailed T.Text T.Text  -- filename, error message
  deriving (Show, Eq)
