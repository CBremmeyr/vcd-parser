{-# LANGUAGE QuasiQuotes #-}

import Data.Either
import Lib
import Test.Hspec
import Text.ParserCombinators.Parsec
import Text.RawString.QQ

main :: IO ()
main = hspec $ do
  
  describe "parseExpr" $ do
    it "Parses entire VCD file" $ do
      parse parseExpr "" 
        [r|
          $date
             Date text. For example: November 11, 2009.
          $end
          $version
             VCD generator tool version info text.
          $end
          $comment
             Any comment text.
          $end
          $timescale 1ps $end
          $scope module logic $end
          $var wire 8 # data $end
          $var wire 1 $ data_valid $end
          $var wire 1 % en $end
          $var wire 1 & rx_en $end
          $var wire 1 ' tx_en $end
          $var wire 1 ( empty $end
          $var wire 1 ) underrun $end
          $upscope $end
          $enddefinitions $end
          $dumpvars
          bxxxxxxxx #
          x$
          0%
          x&
          x'
          1(
          0)
          $end
          #0
          b10000001 #
          0$
          1%
          0&
          1'
          0(
          0)
          #2211
          0'
          #2296
          b0 #
          1$
          #2302
          0$
          #2303
        |]
         `shouldBe` (Right $
          [
            DateExpr "\n             Date text. For example: November 11, 2009.\n          ",
            VersionExpr "\n             VCD generator tool version info text.\n          ",
            CommentExpr "\n             Any comment text.\n          ",
            TimeScaleExpr (TimeScale {val = 1, unit = PS}),
            ModuleExpr (Module {modName = "logic",
                                signals = [Signal {nodeType = Wire, size = 1, symb = ")", name = "underrun"},
                                           Signal {nodeType = Wire, size = 1, symb = "(", name = "empty"},
                                           Signal {nodeType = Wire, size = 1, symb = "'", name = "tx_en"},
                                           Signal {nodeType = Wire, size = 1, symb = "&", name = "rx_en"},
                                           Signal {nodeType = Wire, size = 1, symb = "%", name = "en"},
                                           Signal {nodeType = Wire, size = 1, symb = "$", name = "data_valid"},
                                           Signal {nodeType = Wire, size = 8, symb = "#", name = "data"}],
                                modules = []}),
            EndDefinitionsExpr,
            ValInitExpr [ChangeVector ("#",[X,X,X,X,X,X,X,X]),
                         ChangeScalar ("$",X),
                         ChangeScalar ("%",Lo),
                         ChangeScalar ("&",X),
                         ChangeScalar ("'",X),
                         ChangeScalar ("(",Hi),
                         ChangeScalar (")",Lo)],
            TimeChangeExpr 0,
            ValChangeExpr (ChangeVector ("#",[Hi,Lo,Lo,Lo,Lo,Lo,Lo,Hi])),
            ValChangeExpr (ChangeScalar ("$",Lo)),
            ValChangeExpr (ChangeScalar ("%",Hi)),
            ValChangeExpr (ChangeScalar ("&",Lo)),
            ValChangeExpr (ChangeScalar ("'",Hi)),
            ValChangeExpr (ChangeScalar ("(",Lo)),
            ValChangeExpr (ChangeScalar (")",Lo)),
            TimeChangeExpr 2211,
            ValChangeExpr (ChangeScalar ("'",Lo)),
            TimeChangeExpr 2296,
            ValChangeExpr (ChangeVector ("#",[Lo])),
            ValChangeExpr (ChangeScalar ("$",Hi)),
            TimeChangeExpr 2302,
            ValChangeExpr (ChangeScalar ("$",Lo)),
            TimeChangeExpr 2303
          ]
         )
         
  
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
        `shouldBe` ( Right $
                        ValInitExpr [
                          (ChangeVector ("#", [X, X, X, X, X, X, X, X])),
                          (ChangeScalar ("$", X)),
                          (ChangeScalar ("%", Lo)),
                          (ChangeScalar ("&", X)),
                          (ChangeScalar ("'", X)),
                          (ChangeScalar ("(", Hi)),
                          (ChangeScalar (")", Lo))
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
