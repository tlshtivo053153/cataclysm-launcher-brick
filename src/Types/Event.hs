module Types.Event (
    UIEvent(..)
) where

import qualified Data.Text as T
import Types.Domain

data UIEvent
  = LogMessage T.Text
  | LogEvent T.Text
  | ErrorEvent T.Text
  | CacheHit T.Text
  | InstallFinished (Either ManagerError String)
  | ProfileCreated (Either ManagerError ())
  | BackupCreated (Either ManagerError ())
  | BackupsListed (Either ManagerError [BackupInfo])
  | ModInstallFinished (Either ModHandlerError ModInfo)
  | ModEnableFinished (Either ModHandlerError ())
  | ModDisableFinished (Either ModHandlerError ())
  | AvailableModsListed ([AvailableMod], [ModInfo])
  | ActiveModsListed [ModInfo]
  deriving (Show, Eq)
