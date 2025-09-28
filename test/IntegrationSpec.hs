
module IntegrationSpec (spec) where

import Test.Hspec
import qualified Integration.WorkflowSpec
import qualified Integration.DownloadSpec
import qualified Integration.SandboxSpec

spec :: Spec
spec = describe "Integration Tests" $ do
  describe "Workflow" Integration.WorkflowSpec.spec
  describe "Download" Integration.DownloadSpec.spec
  describe "Sandbox" Integration.SandboxSpec.spec
