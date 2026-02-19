module Types.Event (
    UIEvent(..)
) where

import qualified Data.Text as T
import Types.Domain
import Types.Font (FontInfo, InstalledFont)
import Types.Error (ManagerError)

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
  deriving (Show, Eq)
