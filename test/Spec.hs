module Main (main) where

import Test.Hspec
import qualified ArchiveUtilsSpec
import qualified BackupSystemSpec
import qualified ConfigSpec
import qualified ContentManagerSpec
import qualified FileSystemUtilsSpec
import qualified GitHubIntegrationSpec
import qualified HandleSpec
import qualified IntegrationSpec
import qualified ModHandlerSpec
import qualified ModUtilsSpec
import qualified SandboxControllerSpec

import qualified Events.AppSpec
import qualified Events.AvailableSpec
import qualified Events.InstalledSpec
import qualified Events.ListSpec
import qualified Events.ModsSpec
import qualified Events.SandboxSpec

import qualified GameManager.InstallSpec

import qualified GitHubIntegration.InternalSpec
import qualified LibSpec

-- Note: Types specs are omitted as they are pending

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ArchiveUtils" ArchiveUtilsSpec.spec
  describe "BackupSystem" BackupSystemSpec.spec
  describe "Config" ConfigSpec.spec
  describe "ContentManager" ContentManagerSpec.spec
  describe "FileSystemUtils" FileSystemUtilsSpec.spec
  describe "GitHubIntegration" GitHubIntegrationSpec.spec
  describe "Handle" HandleSpec.spec
  describe "Integration" IntegrationSpec.spec
  describe "ModHandler" ModHandlerSpec.spec
  describe "ModUtils" ModUtilsSpec.spec
  describe "SandboxController" SandboxControllerSpec.spec
  describe "Events.App" Events.AppSpec.spec
  describe "Events.Available" Events.AvailableSpec.spec
  describe "Events.Installed" Events.InstalledSpec.spec
  describe "Events.List" Events.ListSpec.spec
  describe "Events.Mods" Events.ModsSpec.spec
  describe "Events.Sandbox" Events.SandboxSpec.spec
  describe "GameManager.Install" GameManager.InstallSpec.spec
  describe "GitHubIntegration.Internal" GitHubIntegration.InternalSpec.spec
  describe "Lib" LibSpec.spec