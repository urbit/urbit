
import           Data.Version     (makeVersion)
import           Stack2nix.Util
import           Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Stack2nix.Util" $ do
    it "cabal-install version extraction" $ do
      extractVersion "cabal-install version 2.0.0.0" `shouldBe` Just (makeVersion [2, 0, 0, 0])
    it "git version extraction" $ do -- issue #67
      extractVersion "git version 2.11.0 (Apple Git-81)" `shouldBe` Just (makeVersion [2, 11, 0])
    it "cabal2nix version extraction" $ do
      extractVersion "cabal2nix 2.7" `shouldBe` Just (makeVersion [2, 7])
    it "ghc version extraction" $ do
      extractVersion "The Glorious Glasgow Haskell Compilation System, version 8.0.2" `shouldBe` Just (makeVersion [8, 0, 2])
