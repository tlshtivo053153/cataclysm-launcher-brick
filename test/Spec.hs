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
import qualified Integration.FontLinkingSpec
import qualified ModHandlerSpec
import qualified ModUtilsSpec
import qualified SandboxControllerSpec
import qualified SoundpackManagerSpec
import qualified Soundpack.CoreSpec
import qualified Soundpack.InstallSpec
import qualified FontManagerSpec

import qualified Events.AppSpec
import qualified Events.AvailableSpec
import qualified Events.HelpSpec
import qualified Events.InstalledSpec
import qualified Events.ListSpec
import qualified Events.ModSpec
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
  describe "Integration.FontLinking" Integration.FontLinkingSpec.spec
  describe "ModHandler" ModHandlerSpec.spec
  describe "ModUtils" ModUtilsSpec.spec
  describe "SandboxController" SandboxControllerSpec.spec
  describe "SoundpackManager" SoundpackManagerSpec.spec
  describe "Soundpack.Core" Soundpack.CoreSpec.spec
  describe "Soundpack.Install" Soundpack.InstallSpec.spec
  describe "Events.App" Events.AppSpec.spec
  describe "Events.Available" Events.AvailableSpec.spec
  describe "Events.Help" Events.HelpSpec.spec
  describe "Events.Installed" Events.InstalledSpec.spec
  describe "Events.List" Events.ListSpec.spec
  describe "Events.Mod" Events.ModSpec.spec
  describe "Events.Sandbox" Events.SandboxSpec.spec
  describe "GameManager.Install" GameManager.InstallSpec.spec
  describe "GitHubIntegration.Internal" GitHubIntegration.InternalSpec.spec
  describe "Lib" LibSpec.spec
  describe "FontManager" FontManagerSpec.spec