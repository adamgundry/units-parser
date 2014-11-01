{- Test the unit parser
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell #-}

module Tests.Parser where

import Prelude hiding ( lex, exp )

import Text.Parse.Units

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Text.Parsec
import Data.Generics
import Language.Haskell.TH

import Test.Tasty
import Test.Tasty.HUnit

leftOnly :: Either a b -> Maybe a
leftOnly (Left a) = Just a
leftOnly (Right _) = Nothing

----------------------------------------------------------------------
-- TH functions
----------------------------------------------------------------------

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

pprintUnqualified :: (Ppr a, Data a) => a -> String
pprintUnqualified = pprint . stripModules

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

lexTest :: String -> String
lexTest s =
  case lex s of
    Left _     -> "error"
    Right toks -> show toks

lexTestCases :: [(String, String)]
lexTestCases = [ ( "m", "[m]" )
               , ( "", "[]" )
               , ( "m s", "[m,s]" )
               , ( "   m   s ", "[m,s]" )
               , ( "m   ", "[m]" )
               , ( "   m", "[m]" )
               , ( "( m  /s", "[(,m,/,s]" )
               , ( "!", "error" )
               , ( "1 2 3", "[1,2,3]" )
               , ( "  ", "[]" )
               ]

lexTests :: TestTree
lexTests = testGroup "Lexer" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ lexTest str @?= out) lexTestCases

----------------------------------------------------------------------
-- Unit strings
----------------------------------------------------------------------

unitStringTestCases :: [(String, String)]
unitStringTestCases = [ ("m", "Meter")
                      , ("s", "Second")
                      , ("min", "Minute")
                      , ("km", "Kilo :@ Meter")
                      , ("mm", "Milli :@ Meter")
                      , ("kmin", "Kilo :@ Minute")
                      , ("dam", "error")   -- ambiguous!
                      , ("damin", "Deca :@ Minute")
                      , ("ms", "Milli :@ Second")
                      , ("mmin", "Milli :@ Minute")
                      , ("mmm", "error")
                      , ("mmmin", "error")
                      , ("sm", "error")
                      , ("", "error")
                      , ("dak", "error")
                      , ("das", "Deca :@ Second")
                      , ("ds", "Deci :@ Second")
                      , ("daam", "Deca :@ Ampere")
                      , ("kam", "Kilo :@ Ampere")
                      , ("dm", "Deci :@ Meter")
                      ]

parseUnitStringTest :: String -> String
parseUnitStringTest s =
  case flip runReader testSymbolTable $ runParserT unitStringParser () "" s of
    Left _ -> "error"
    Right exp -> show exp

unitStringTests :: TestTree
unitStringTests = testGroup "UnitStrings" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitStringTest str @?= out)
    unitStringTestCases

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

mkSymbolTableTests :: TestTree
mkSymbolTableTests = testGroup "mkSymbolTable"
  [ testCase "Unambiguous1" (Map.keys (prefixTable testSymbolTable) @?= ["d","da","k","m"])
  -- , testCase "Unambiguous2" (Map.keys (unitTable testSymbolTable) @?= ["am","m","min","s"])
  , testCase "AmbigPrefix" (leftOnly (mkSymbolTable [("a",''Milli),("a",''Centi)] ([] :: [(String,Name)])) @?= Just "The label `a' is assigned to the following meanings:\n[\"Tests.Parser.Milli\",\"Tests.Parser.Centi\"]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "AmbigUnit" (leftOnly (mkSymbolTable ([] :: [(String,Name)]) [("m",''Meter),("m",''Minute)]) @?= Just "The label `m' is assigned to the following meanings:\n[\"Tests.Parser.Meter\",\"Tests.Parser.Minute\"]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "MultiAmbig" (leftOnly (mkSymbolTable [("a",''Milli),("b",''Centi),("b",''Deci),("b",''Kilo),("c",''Atto),("c",''Deca)] [("m",''Meter),("m",''Minute),("s",''Second)]) @?= Just "The label `b' is assigned to the following meanings:\n[\"Tests.Parser.Centi\",\"Tests.Parser.Deci\",\"Tests.Parser.Kilo\"]\nThe label `c' is assigned to the following meanings:\n[\"Tests.Parser.Atto\",\"Tests.Parser.Deca\"]\nThis is ambiguous. Please fix before building a unit parser.")
                                                                                                ]

testSymbolTable :: SymbolTable Name Name
Right testSymbolTable =
   mkSymbolTable (stripModules [ ("k", ''Kilo)
                               , ("da", ''Deca)
                               , ("m", ''Milli)
                               , ("d", ''Deci) ])
                 (stripModules [ ("m", ''Meter)
                               , ("s", ''Second)
                               , ("min", ''Minute)
                               , ("am", ''Ampere) ])

data Kilo   = Kilo
data Deca   = Deca
data Centi  = Centi
data Milli  = Milli
data Deci   = Deci
data Atto   = Atto

data Meter  = Meter
data Second = Second
data Minute = Minute
data Ampere = Ampere

----------------------------------------------------------------------
-- Conclusion
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Parser"
  [ lexTests
  , mkSymbolTableTests
  , unitStringTests
  ]

main :: IO ()
main = defaultMain tests
