module Lib where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TimeUnit = S | MS | US | NS | PS | FS deriving Show
data TimeScale = TimeScale {
                            val  :: Int,
                            unit :: TimeUnit
                           } deriving Show

data LogicLevel = X | Z | Hi | Lo
-- TODO: need data(s) for holding actual signal values

data NodeType   = Wire | Reg deriving Show
data Signal = Signal {
                        nodeType :: NodeType,
                        size :: Int,
                        symb :: String,
                        name :: String
                     } deriving Show

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

-- Consume leading whitespace if present
eatSpaces :: Parser a -> Parser a
eatSpaces p = do
        _ <- many space
        p

-- Consume leading whitespace, requires at least 1 char 
eatSpaces1 :: Parser a -> Parser a
eatSpaces1 p = do
        _ <- many1 space
        p

parseNodeType :: Parser NodeType
parseNodeType =  (const Wire <$> (string "wire"))
             <|> (const Reg  <$> (string "reg" ))

parseInt :: Parser Int
parseInt = read <$> (many1 digit)

-- Gets chars upto next whitespace
parseWord :: Parser String
--parseWord = manyTill anyChar (try space)
parseWord = manyTill anyChar (lookAhead space)

parseSignal :: Parser Signal 
parseSignal = do
        _   <- eatSpaces  $ string "$var"
        nt  <- eatSpaces1 parseNodeType
        s   <- eatSpaces1 parseInt
        sym <- eatSpaces1 parseWord
        n   <- eatSpaces1 parseWord
        _   <- eatSpaces1 $ string "$end"
        pure $ Signal {
                       nodeType = nt,
                       size = s,
                       symb = sym,
                       name = n 
                      }

-- Get string between `$sectionTag` and `$end`
-- use to parse $date, $version, and $comment sections
parseTextSection :: String -> Parser String
parseTextSection sectionTag =  eatSpaces $
                               between (string ('$':sectionTag) )
                                       (string "$end")
                                       (many1 anyChar)

parseTimeScale :: Parser TimeScale
parseTimeScale = do
            _ <- eatSpaces  $ string "$timescale"
            v <- eatSpaces1 parseInt
            u <- eatSpaces  parseTimeUnit
                 where
                    parseTimeUnit =  (const S  <$> (string "s" ))
                                 <|> (const MS <$> (string "ms"))
                                 <|> (const US <$> (string "us"))
                                 <|> (const NS <$> (string "ns"))
                                 <|> (const PS <$> (string "ps"))
                                 <|> (const FS <$> (string "fs"))
            _ <- eatSpaces1 $ string "$end"
            pure $ TimeScale {
                                val  = v,
                                unit = u
                             }

