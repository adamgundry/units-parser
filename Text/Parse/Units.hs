{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, FlexibleContexts, RankNTypes,
             Safe, DeriveGeneric, DeriveDataTypeable, CPP, StandaloneDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parse.Units
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines a parser for unit expressions.  The syntax for
-- these expressions is like F#'s. There are four arithmetic operators
-- (@*@, @/@, @^@, and juxtaposition).  Exponentiation binds the
-- tightest, and it allows an integer to its right (possibly with
-- minus signs and parentheses). Next tightest is juxtaposition, which
-- indicates multiplication. Because juxtaposition binds tighter than
-- division, the expressions @m/s^2@ and @m/s s@ are
-- equivalent. Multiplication and division bind the loosest and are
-- left-associative, meaning that @m/s*s@ is equivalent to @(m/s)*s@,
-- probably not what you meant. Parentheses in unit expressions are
-- allowed, of course.
--
-- Within a unit string (that is, a unit with an optional prefix),
-- there may be ambiguity. If a unit string can be interpreted as a
-- unit without a prefix, that parsing is preferred. Thus, @min@ would
-- be minutes, not milli-inches (assuming appropriate prefixes and
-- units available.) There still may be ambiguity between unit
-- strings, even interpreting the string as a prefix and a base
-- unit. If a unit string is amiguous in this way, it is rejected.
-- For example, if we have prefixes @da@ and @d@ and units @m@ and
-- @am@, then @dam@ is ambiguous like this.
-----------------------------------------------------------------------------

module Text.Parse.Units (
  -- * Parsing units
  UnitExp(..), parseUnit,

  -- * Symbol tables
  SymbolTable(..), PrefixTable, UnitTable, mkSymbolTable,
  unsafeMkSymbolTable, universalSymbolTable,

  lex, unitStringParser  -- these are pruned from the Haddock output
  ) where

import Prelude hiding ( lex, div )

import GHC.Generics (Generic)
import Text.Parsec         hiding ( tab )
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Monad.Reader
import Control.Arrow       hiding ( app)
import Data.Data (Data)
import Data.Maybe
import Data.Char

#if __GLASGOW_HASKELL__ < 709
import Data.Typeable ( Typeable )
#endif

----------------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------------

-- copied from GHC
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

----------------------------------------------------------------------
-- Extra parser combinators
----------------------------------------------------------------------

-- | @experiment p@ runs @p@. If @p@ succeeds, @experiment@ returns the
-- result of running @p@. If @p@ fails, then @experiment@ returns @Nothing@.
-- In either case, no input is consumed and @experiment@ never fails.
experiment :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
experiment = lookAhead . optionMaybe . try

consumeAll :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
consumeAll p = do
  result <- p
  eof
  return result

nochar :: Stream s m Char => Char -> ParsecT s u m ()
nochar = void . char

----------------------------------------------------------------------
-- Datatypes
----------------------------------------------------------------------

data Op = NegO | MultO | DivO | PowO | OpenP | CloseP

instance Show Op where
  show NegO    = "-"
  show MultO   = "*"
  show DivO    = "/"
  show PowO    = "^"
  show OpenP   = "("
  show CloseP  = ")"

data Token = UnitT String
           | NumberT Integer
           | OpT Op

instance Show Token where
  show (UnitT s)   = s
  show (NumberT i) = show i
  show (OpT op)    = show op

-- | Parsed unit expressions, parameterized by a prefix identifier type and
-- a unit identifier type
data UnitExp pre u = Unity                     -- ^ "1"
                   | Unit (Maybe pre) u        -- ^ a unit with, perhaps, a prefix
                   | Mult (UnitExp pre u) (UnitExp pre u)
                   | Div (UnitExp pre u) (UnitExp pre u)
                   | Pow (UnitExp pre u) Integer
                   deriving (Eq, Ord, Generic, Data)

#if __GLASGOW_HASKELL__ < 709
deriving instance Typeable UnitExp
#endif

instance (Show pre, Show u) => Show (UnitExp pre u) where
  show Unity               = "1"
  show (Unit (Just pre) u) = show pre ++ " :@ " ++ show u
  show (Unit Nothing u)    = show u
  show (Mult e1 e2)        = "(" ++ show e1 ++ " :* " ++ show e2 ++ ")"
  show (Div e1 e2)         = "(" ++ show e1 ++ " :/ " ++ show e2 ++ ")"
  show (Pow e i)           = show e ++ " :^ " ++ show i

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

type Lexer = Parser

unitL :: Lexer Token
unitL = UnitT `fmap` (many1 letter)

opL :: Lexer Token
opL = fmap OpT $
      do { nochar '-'; return NegO    }
  <|> do { nochar '*'; return MultO   }
  <|> do { nochar '/'; return DivO    }
  <|> do { nochar '^'; return PowO    }
  <|> do { nochar '('; return OpenP   }
  <|> do { nochar ')'; return CloseP  }

numberL :: Lexer Token
numberL = (NumberT . read) `fmap` (many1 digit)

lexer1 :: Lexer Token
lexer1 = unitL <|> opL <|> numberL

lexer :: Lexer [Token]
lexer = do
  spaces
  choice
    [ do eof <?> ""
         return []
    , do tok <- lexer1
         spaces
         toks <- lexer
         return (tok : toks)
    ]

lex :: String -> Either ParseError [Token]
lex = parse lexer ""

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

-- | A finite mapping from prefix spellings to prefix identifiers (of
-- unspecified type @pre@). All prefix spellings must be strictly alphabetic.
type PrefixTable pre = Map.Map String pre

-- | A mapping from unit spellings to unit identifiers (of unspecified type
-- @u@). All unit spellings must be strictly alphabetic.
type UnitTable u = String -> Maybe u

-- | A "symbol table" for the parser, mapping prefixes and units to their
-- representations.
data SymbolTable pre u = SymbolTable { prefixTable :: PrefixTable pre
                                     , unitTable   :: UnitTable u
                                     } deriving (Generic)

-- | Build a 'Map' from an association list, checking for ambiguity
unambFromList :: (Ord a, Show b) => [(a,b)] -> Either [(a,[String])] (Map.Map a b)
unambFromList list =
  let multimap      = MM.fromList list
      assocs        = MM.assocs multimap
      (errs, goods) = partitionWith (\(key, vals) ->
                                       case vals of
                                         [val] -> Right (key, val)
                                         _     -> Left (key, map show vals)) assocs
      result        = Map.fromList goods
  in
  if null errs then Right result else Left errs

-- | Build a symbol table from prefix mappings and unit mappings. The prefix mapping
-- can be empty. This function checks to make sure that the strings are not
-- inherently ambiguous and are purely alphabetic.
mkSymbolTable :: (Show pre, Show u)
              => [(String, pre)]   -- ^ Association list of prefixes
              -> [(String, u)]     -- ^ Association list of units
              -> Either String (SymbolTable pre u)
mkSymbolTable prefixes units =
  let bad_strings = filter (not . all isLetter) (map fst prefixes ++ map fst units) in
  if not (null bad_strings)
  then Left $ "All prefixes and units must be composed entirely of letters.\nThe following are illegal: " ++ show bad_strings
  else
  let result = do
        prefixTab <- unambFromList prefixes
        unitTab   <- unambFromList units
        return $ SymbolTable { prefixTable = prefixTab, unitTable = flip Map.lookup unitTab }
  in left ((++ error_suffix) . concatMap mk_error_string) result
  where
    mk_error_string :: Show x => (String, [x]) -> String
    mk_error_string (k, vs) =
      "The label `" ++ k ++ "' is assigned to the following meanings:\n" ++
      show vs ++ "\n"
    error_suffix = "This is ambiguous. Please fix before building a unit parser."

-- | Make a symbol table without checking for ambiguity or non-purely
-- alphabetic strings.  The prefixes must be a (potentially empty)
-- finite map, but the units mapping need not be finite.
-- Note that this is unsafe in that the resulting parser may behave
-- unpredictably. It surely won't launch the rockets, though.
unsafeMkSymbolTable :: PrefixTable pre -> UnitTable u -> SymbolTable pre u
unsafeMkSymbolTable = SymbolTable

-- | A symbol table that accepts all unit strings, but supports no prefixes.
universalSymbolTable :: SymbolTable a String
universalSymbolTable = SymbolTable Map.empty Just


----------------------------------------------------------------------
-- Unit string parser
----------------------------------------------------------------------

-- We assume that no symbol table is inherently ambiguous!

type GenUnitStringParser pre u = ParsecT String () (Reader (SymbolTable pre u))
type UnitStringParser_UnitExp =
  forall pre u. (Show pre, Show u) => GenUnitStringParser pre u (UnitExp pre u)

-- parses just a unit (no prefix)
justUnitP :: GenUnitStringParser pre u u
justUnitP = do
  full_string <- getInput
  units <- asks unitTable
  case units full_string of
    Nothing -> fail (full_string ++ " does not match any known unit")
    Just u  -> return u

-- parses a unit and prefix, failing in the case of ambiguity
prefixUnitP :: UnitStringParser_UnitExp
prefixUnitP = do
  prefixTab <- asks prefixTable
  let assocs = Map.assocs prefixTab  -- these are in the right order
  results <- catMaybes `liftM` mapM (experiment . parse_one) assocs
  full_string <- getInput
  case results of
    [] -> fail $ "No known interpretation for " ++ full_string
    [(pre_name, unit_name)] ->
      return $ Unit (Just pre_name) unit_name
    lots -> fail $ "Multiple possible interpretations for " ++ full_string ++ ":\n" ++
                   (concatMap (\(pre_name, unit_name) ->
                                 "  " ++ show pre_name ++
                                 " :@ " ++ show unit_name ++ "\n") lots)
  where
    parse_one :: (String, pre) -> GenUnitStringParser pre u (pre, u)
    parse_one (pre, name) = do
      void $ string pre
      unit_name <- justUnitP
      return (name, unit_name)

-- parse a unit string
unitStringParser :: UnitStringParser_UnitExp
unitStringParser = try (Unit Nothing `liftM` justUnitP) <|> prefixUnitP

----------------------------------------------------------------------
-- Unit expression parser
----------------------------------------------------------------------

type GenUnitParser pre u = ParsecT [Token] () (Reader (SymbolTable pre u))
type UnitParser a = forall pre u. GenUnitParser pre u a
type UnitParser_UnitExp =
  forall pre u. (Show pre, Show u) => GenUnitParser pre u (UnitExp pre u)

-- move a source position past a token
updatePosToken :: SourcePos -> Token -> [Token] -> SourcePos
updatePosToken pos (UnitT unit_str) _ = updatePosString pos unit_str
updatePosToken pos (NumberT i) _      = updatePosString pos (show i)
updatePosToken pos (OpT _) _          = incSourceColumn pos 1

-- parse a Token
uToken :: (Token -> Maybe a) -> UnitParser a
uToken x = tokenPrim show updatePosToken x

-- consume an lparen
lparenP :: UnitParser ()
lparenP = uToken $ \case
  OpT OpenP -> Just ()
  _         -> Nothing

-- consume an rparen
rparenP :: UnitParser ()
rparenP = uToken $ \case
  OpT CloseP -> Just ()
  _          -> Nothing

-- parse a unit string
unitStringP :: String -> UnitParser_UnitExp
unitStringP str = do
  symbolTable <- ask
  case flip runReader symbolTable $ runParserT unitStringParser () "" str of
    Left err -> fail (show err)
    Right e  -> return e

-- parse a number, possibly negated and nested in parens
numP :: UnitParser Integer
numP =
  do lparenP
     n <- numP
     rparenP
     return n
  <|>
  do uToken $ \case
       OpT NegO -> Just ()
       _        -> Nothing
     negate `liftM` numP
  <|>
  do uToken $ \case
       NumberT i -> Just i
       _         -> Nothing

-- parse an exponentiation, like "^2"
powP :: GenUnitParser pre u (UnitExp pre u -> UnitExp pre u)
powP = option id $ do
  uToken $ \case
    OpT PowO -> Just ()
    _        -> Nothing
  n <- numP
  return $ flip Pow n

-- parse a unit, possibly with an exponent
unitP :: UnitParser_UnitExp
unitP =
  do n <- numP
     case n of
       1 -> return Unity
       _ -> unexpected $ "number " ++ show n
  <|>
  do unit_str <- uToken $ \case
       UnitT unit_str -> Just unit_str
       _              -> Nothing
     u <- unitStringP unit_str
     maybe_pow <- powP
     return $ maybe_pow u

-- parse a "unit factor": either a juxtaposed sequence of units
-- or a paranthesized unit exp.
unitFactorP :: UnitParser_UnitExp
unitFactorP =
  do lparenP
     unitExp <- parser
     rparenP
     return unitExp
  <|>
  (foldl1 Mult `liftM` many1 unitP)

-- parse * or /
opP :: GenUnitParser pre u (UnitExp pre u -> UnitExp pre u -> UnitExp pre u)
opP = uToken $ \case
        OpT MultO -> Just Mult
        OpT DivO  -> Just Div
        _         -> Nothing

-- parse a whole unit expression
parser :: UnitParser_UnitExp
parser = chainl unitFactorP opP Unity

-- | Parse a unit expression, interpreted with respect the given symbol table.
-- Returns either an error message or the successfully-parsed unit expression.
parseUnit :: (Show pre, Show u)
          => SymbolTable pre u -> String -> Either String (UnitExp pre u)
parseUnit tab s = left show $ do
  toks <- lex s
  flip runReader tab $ runParserT (consumeAll parser) () "" toks
