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
  | ProfileCreated (Either ManagerError SandboxProfile)
  | BackupCreated (Either ManagerError ())
  | BackupsListed (Either ManagerError [BackupInfo])
  | ModInstallFinished (Either ModHandlerError ModInfo)
  | ModEnableFinished (Either ModHandlerError ())
  | ModDisableFinished (Either ModHandlerError ())
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
  | InstalledFontsListed [InstalledFont]
  deriving (Show, Eq)
