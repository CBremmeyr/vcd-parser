{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Lib where

import Data.Char
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TimeUnit = S | MS | US | NS | PS | FS deriving (Show, Eq)

data TimeScale = TimeScale
  { val :: Int,
    unit :: TimeUnit
  }
  deriving (Show, Eq)

data LogicLevel = X | Z | Hi | Lo deriving (Show, Eq)

data ValChange
  = ChangeScalar (Tag, LogicLevel)
  | ChangeVector (Tag, [LogicLevel])
  deriving (Show, Eq)

type Tag = String

data NodeType = Wire | Reg deriving (Show, Eq)

data Signal = Signal
  { nodeType :: NodeType,
    size :: Int,
    symb :: String,
    name :: String
  }
  deriving (Show, Eq)

data Module = Module
  { modName :: String,
    signals :: [Signal],
    modules :: [Module]
  }
  deriving (Show, Eq)

data ModOrSig
  = Sig Signal
  | Mod Module
  deriving (Show, Eq)

data Expr
  = ModuleExpr Module
  | DateExpr
  | VersionExpr
  | CommentExpr
  | TimeScaleExpr
  | EndDefinitionsExpr
  | ValInitExpr [ValChange]
  | ValChangeExpr ValChange
  | TimeChangeExpr Int
  deriving (Show, Eq)

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
parseNodeType =
  (Wire <$ string "wire")
    <|> (Reg <$ string "reg")

parseInt :: Parser Int
parseInt = read <$> many1 digit

-- Gets chars upto next whitespace
parseWord :: Parser String
parseWord = many (noneOf " \t\n")

-- Parse signal tag
parseTag :: Parser Tag
parseTag = do parseWord

-- Get string between `$sectionTag` and `$end`
-- use to parse $date, $version, and $comment sections
parseTextSection :: String -> Parser String
parseTextSection sectionTag =
  eatSpaces $
    between
      (string ('$' : sectionTag))
      (string "$end")
      (many1 anyChar)

-- Get timescale value and units
-- between `$timescale` and `$end`
parseTimeScale :: Parser TimeScale
parseTimeScale = do
  _ <- eatSpaces $ string "$timescale"
  v <- eatSpaces1 parseInt
  u <- eatSpaces parseTimeUnit
  _ <- eatSpaces1 $ string "$end"
  pure $
    TimeScale
      { val = v,
        unit = u
      }
  where
    parseTimeUnit =
      (S <$ string "s")
        <|> (MS <$ string "ms")
        <|> (US <$ string "us")
        <|> (NS <$ string "ns")
        <|> (PS <$ string "ps")
        <|> (FS <$ string "fs")

parseModOrSig :: Parser ModOrSig
parseModOrSig =
      (Sig <$> try parseSignal)
  <|> (Mod <$> try parseModule)

parseSignal :: Parser Signal
parseSignal = do
  _   <- eatSpaces $ string "$var"
  nt  <- eatSpaces1 parseNodeType
  s   <- eatSpaces1 parseInt
  sym <- eatSpaces1 parseWord
  n   <- eatSpaces1 parseWord
  _   <- eatSpaces1 $ string "$end"
  pure $
    Signal
      { nodeType = nt,
        size = s,
        symb = sym,
        name = n
      }

parseModule :: Parser Module
parseModule = do
  _ <- eatSpaces $ string "$scope"
  _ <- eatSpaces1 $ string "module"
  n <- eatSpaces1 parseWord
  _ <- eatSpaces1 $ string "$end"
  x <- many1 parseModOrSig
  _ <- eatSpaces1 $ string "$upscope"
  _ <- eatSpaces1 $ string "$end"
  pure $
    Module
      { modName = n,
        signals = extractSig x,
        modules = extractMod x
      }
  where
    extractSig :: [ModOrSig] -> [Signal]
    extractSig = foldl littleSigHelper []

    extractMod :: [ModOrSig] -> [Module]
    extractMod = foldl littleModHelper []

    littleSigHelper :: [Signal] -> ModOrSig -> [Signal]
    littleSigHelper acc (Sig s) = s : acc
    littleSigHelper acc (Mod _) = acc

    littleModHelper :: [Module] -> ModOrSig -> [Module]
    littleModHelper acc (Sig _) = acc
    littleModHelper acc (Mod m) = m : acc

parseEndDefinitions :: Parser Expr
parseEndDefinitions = do
  _ <- eatSpaces $ string "$enddefinitions"
  _ <- eatSpaces1 $ string "$end"
  pure EndDefinitionsExpr

-- Parse out `$dumpvars` and initial values for signals and `$end`
parseValInit :: Parser [Expr]
parseValInit = do
  _        <- eatSpaces1 $ string "$dumpvars"
  initVals <- many initValsHelper
  _        <- eatSpaces1 $ string "$end"
  return initVals
  where
    initValsHelper :: Parser Expr
    initValsHelper = ValChangeExpr <$> parseValChange

parseValChange :: Parser ValChange
parseValChange = try parseVecLogicLevel <|> try parseScalarLogicLevel

-- Parse scalar LogicLevels
parseScalarLogicLevel :: Parser ValChange
parseScalarLogicLevel = do
  s <- eatSpaces1 parseLogicLevel
  t <- eatSpaces parseTag
  pure $ ChangeScalar (t, s)

-- Parse out base char for vectors and list of LogicLevels
parseVecLogicLevel :: Parser ValChange
parseVecLogicLevel = do
  _ <- eatSpaces1 $ try $ char 'b'
  v <- many parseLogicLevel
  t <- eatSpaces parseTag
  pure $ ChangeVector (t, v)

-- Parse logic level chars into LogicLevel enum
parseLogicLevel :: Parser LogicLevel
parseLogicLevel =
  (char '1') *> pure Hi
    <|> (char '0') *> pure Lo
    <|> (char 'x') *> pure X
    <|> (char 'X') *> pure X
    <|> (char 'z') *> pure Z
    <|> (char 'Z') *> pure Z
