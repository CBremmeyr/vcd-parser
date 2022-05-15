# vcd-parser
Parses a Value Change Dump file.

                    +------------+
myWaveform.vcd ---> | vcd-parser | ---> VcdTree
                    +------------+

data LogicLevel = X | Z | Hi | Lo
data NodeType = Wire | Reg
data Signal = Signal {
                       nodeType :: NodeType,
                       size :: Int,
                       symb :: String,
                       name :: String
                     }

data Module = Module {
                       signals :: [Signal],
                       modules :: [Module]
                     }

data Expr = ModuleExpr Module
          | DateExpr
          | VersionExpr
          | CommentExpr
          | TimeScaleExpr
          | ValInitExpr
          | ValChangesExpr

parse :: String -> Parser Expr

data VcdTree = Leaf Signal | Branch [(String, VcdTree)]

What should the output data structure look like?
- heirarcy
    - tree `data Tree a = Leaf a | Branch [Tree a]`
        - an `a` can be either another module or signal
- signals
    - value at every point in time held in a list?
        - long list which is likely mostly repeated values
    - list of ()'s, (timestamp, value) where `value`
    is un-changed from t=`timestamp` to t<`next_timestamp`
        - data Signal = (String, [(Timestamp, LogicLevel)])
            - data Timestamp = Integer
    - table of values, row for ?timestamps?, row for each signal
- misc header data
    - just output string(s)?
