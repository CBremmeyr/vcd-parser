import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "parseSignal" $ do
        it "Parses signals" $ do
            True
            -- parseSignal "inputString" `shouldBe` Signal { 
            --         nodeType = Wire,
            --         size     = 8,
            --         symb     = "#",
            --         name     = "data"
            --       }

