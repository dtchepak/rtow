module PpmSpec (spec) where

import qualified RayTracingOneWeekend as RT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B
import Test.Hspec

spec :: Spec
spec = do
    describe "Hello World example" $ do
        it "should match example" $ do
            let expectedStart = (B.toLazyByteString . B.string7 . unlines)
                    [ "P3"
                    , "256 256"
                    , "255"
                    , "0 255 63"
                    , "1 255 63"
                    , "2 255 63"
                    , "3 255 63"
                    , "4 255 63"
                    , "5 255 63"
                    , "6 255 63"
                    , "7 255 63"
                    , "8 255 63"
                    , "9 255 63"
                    ]
            let actualStart = LBS.take (LBS.length expectedStart) (RT.pack RT.example2_1)
            actualStart `shouldBe` expectedStart

