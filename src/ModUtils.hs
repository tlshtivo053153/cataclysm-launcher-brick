{-# LANGUAGE OverloadedStrings #-}

module ModUtils (combineMods) where

import Data.List (nubBy)
import Data.Function (on)
import qualified Data.Text as T
import Types

-- | Combines mod sources from config and installed mods from the filesystem
-- into a single list for the UI. It ensures that there are no duplicates.
-- This function is pure and can be tested independently.
combineMods :: [ModSourceInfo] -> [ModInfo] -> [AvailableMod]
combineMods modSources installedMods = nubBy ((==) `on` (msiRepositoryName . amSource)) (sourceMods ++ installedOnlyMods)
  where
    isInstalled :: ModSourceInfo -> Bool
    isInstalled msi = any (\im -> miName im == msiRepositoryName msi) installedMods

    sourceMods :: [AvailableMod]
    sourceMods = map (\msi -> AvailableMod msi (isInstalled msi)) modSources

    installedOnly :: [ModInfo]
    installedOnly = filter (\im -> not (any (\msi -> msiRepositoryName msi == miName im) modSources)) installedMods

    installedOnlyMods :: [AvailableMod]
    installedOnlyMods = map modInfoToAvailableMod installedOnly

    modInfoToAvailableMod :: ModInfo -> AvailableMod
    modInfoToAvailableMod mi = AvailableMod
        { amSource = ModSourceInfo
            { msiName = miName mi
            , msiRepositoryName = miName mi
            , msiUrl = getSourceUrl (miSource mi)
            , msiType = getSourceType (miSource mi)
            }
        , amIsInstalled = True
        }

    getSourceUrl :: ModSource -> T.Text
    getSourceUrl (ModSource url) = url

    getSourceType :: ModSource -> ModDistributionType
    getSourceType (ModSource url) = if "github.com" `T.isInfixOf` url then GitHub else TarGz
