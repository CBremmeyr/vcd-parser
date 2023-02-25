{-# LANGUAGE QuasiQuotes #-}

import Data.Either
import Lib
import Test.Hspec
import Text.ParserCombinators.Parsec
import Text.RawString.QQ

main :: IO ()
main = hspec $ do
  -- parseValInit :: Parser [Expr]
  describe "parseValInit" $ do
    it "Parses inital value block" $ do
      parse
        parseValInit
        ""
        [r| $dumpvars
            bxxxxxxxx #
            x$
            0%
            x&
            x'
            1(
            0)
            $end
        |]
        `shouldBe` ( Right
                       [ ValChangeExpr (ChangeVector ("#", [X, X, X, X, X, X, X, X])),
                         ValChangeExpr (ChangeScalar ("$", X)),
                         ValChangeExpr (ChangeScalar ("%", Lo)),
                         ValChangeExpr (ChangeScalar ("&", X)),
                         ValChangeExpr (ChangeScalar ("'", X)),
                         ValChangeExpr (ChangeScalar ("(", Hi)),
                         ValChangeExpr (ChangeScalar (")", Lo))
                       ]
                   )

  describe "parseScalarLogicLevel" $ do
    it "Parses scalar of logic level 0" $ do
      parse parseScalarLogicLevel "" " 0 %"
        `shouldBe` Right
          (ChangeScalar ("%", Lo))
    it "Parses scalar of logic level 1" $ do
      parse parseScalarLogicLevel "" " 1 ^"
        `shouldBe` Right
          (ChangeScalar ("^", Hi))

  describe "parseVecLogicLevel" $ do
    it "Parses vector of logic levels" $ do
      parse parseVecLogicLevel "" " b01zZxX10 )"
        `shouldBe` Right
          (ChangeVector (")", [Lo, Hi, Z, Z, X, X, Hi, Lo]))

  describe "parseLogicLevel" $ do
    it "Parses logic level char" $ do
      parse parseLogicLevel "" "0" `shouldBe` Right Lo
      parse parseLogicLevel "" "1" `shouldBe` Right Hi
      parse parseLogicLevel "" "x" `shouldBe` Right X
      parse parseLogicLevel "" "X" `shouldBe` Right X
      parse parseLogicLevel "" "z" `shouldBe` Right Z
      parse parseLogicLevel "" "Z" `shouldBe` Right Z
      parse parseLogicLevel "" "2" `shouldSatisfy` isLeft

  describe "parseSignal" $ do
    it "Parses signals" $ do
      parse parseSignal "" "$var wire 8 # data $end"
        `shouldBe` Right
          ( Signal
              { nodeType = Wire,
                size = 8,
                symb = "#",
                name = "data"
              }
          )

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
