{-# LANGUAGE QuasiQuotes #-}

import Lib
import Test.Hspec
import Text.ParserCombinators.Parsec
import Text.RawString.QQ

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

  describe "parseModule" $ do
    it "Parses a module with signals inside" $ do
      let actual =
            parse
              parseModule
              ""
              [r| $scope module myMod $end
                                $var wire 128 # mySignal $end
                                $var reg 3 & yourSignal $end
                            $upscope $end |]
          expected =
            Right
              ( Module
                  { modName = "myMod",
                    signals =
                      [ Signal
                          { nodeType = Reg,
                            size = 3,
                            symb = "&",
                            name = "yourSignal"
                          },
                        Signal
                          { nodeType = Wire,
                            size = 128,
                            symb = "#",
                            name = "mySignal"
                          }
                      ],
                    modules = []
                  }
              )
       in actual `shouldBe` expected

    it "Parses nested modules with signals inside" $ do
      let actual =
            parse
              parseModule
              ""
              [r| $scope module myMod $end
                                $var wire 128 # mySignal $end

                                $scope module innerMod $end
                                  $var reg  9 ^ niner $end
                                  $var wire 2 ! twoer $end
                                $upscope $end
                                $var reg 3 & yourSignal $end
                              $upscope $end |]
          expected =
            Right
              ( Module
                  { modName = "myMod",
                    signals =
                      [ Signal
                          { nodeType = Reg,
                            size = 3,
                            symb = "&",
                            name = "yourSignal"
                          },
                        Signal {nodeType = Wire, size = 128, symb = "#", name = "mySignal"}
                      ],
                    modules =
                      [ Module
                          { modName = "innerMod",
                            signals =
                              [ Signal
                                  { nodeType = Wire,
                                    size = 2,
                                    symb = "!",
                                    name = "twoer"
                                  },
                                Signal
                                  { nodeType = Reg,
                                    size = 9,
                                    symb = "^",
                                    name = "niner"
                                  }
                              ],
                            modules = []
                          }
                      ]
                  }
              )
       in actual `shouldBe` expected
