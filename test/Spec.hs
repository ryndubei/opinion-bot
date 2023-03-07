import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lib" $
    describe "DataChannel" $ do
      it "can manage state" $ do
        dataChannel <- spawnDataManager (+) 0
        send 1 dataChannel
        send 1 dataChannel
        reply1 <- request dataChannel
        reply1 `shouldBe` 2
