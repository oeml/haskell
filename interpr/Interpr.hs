{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List as L (unfoldr, stripPrefix, find)
import Data.Map as M (Map, fromList, lookup)
import Data.Maybe (isJust)
import Control.Monad (join)

data Token = TokenOpenPar
           | TokenClosePar
           | TokenPlus
           | TokenMinus
           | TokenAsterisk
           | TokenSlash
           | TokenPercent
           | TokenWedge
           | TokenAbs
           | TokenInt Int
           | TokenIdent String
  deriving (Show)

type TokenSelector = String -> Maybe (String, String)
type TokenReader = String -> Token
type TokenAcceptor = (TokenSelector, TokenReader)
type FixedTokenDescriptor = (String, Token)
type CharCategoryTokenDescriptor = ((Char -> Bool), String -> Token)

fixedTokenDescriptors :: [FixedTokenDescriptor]
fixedTokenDescriptors = [
  ("(",    TokenOpenPar),
  (")",    TokenClosePar),
  ("+",    TokenPlus),
  ("-",    TokenMinus),
  ("*",    TokenAsterisk),
  ("/",    TokenSlash),
  ("%",    TokenPercent),
  ("^",    TokenWedge),
  ("abs",  TokenAbs)
  ]

makeFixedTokenAcceptor :: FixedTokenDescriptor -> TokenAcceptor
makeFixedTokenAcceptor (s, t) = (fmap (s,) . (stripPrefix s), const t)

fixedTokenAcceptors = map makeFixedTokenAcceptor fixedTokenDescriptors

charCategoryTokenDescriptors :: [CharCategoryTokenDescriptor]
charCategoryTokenDescriptors = [
  (isDigit, TokenInt . read),
  (isAlpha, TokenIdent)
  ]

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty [] = Nothing
maybeNonEmpty xs = Just xs

makeCharCategoryTokenAcceptor :: CharCategoryTokenDescriptor -> TokenAcceptor
makeCharCategoryTokenAcceptor (p, f) = (\s -> let (s1, s2) = span p s in fmap (,s2) $ maybeNonEmpty s1, f)

charCategoryTokenAcceptors = map makeCharCategoryTokenAcceptor charCategoryTokenDescriptors

tokenAcceptors = fixedTokenAcceptors ++ charCategoryTokenAcceptors

acceptToken :: String -> Maybe (Token, String)
acceptToken s = join $ find isJust $ map (\(f, g) -> fmap (first g) $ f s) tokenAcceptors

tokenize :: String -> [Token]
tokenize = concat . map (unfoldr acceptToken) . words

data UnOp = UnOpNegate
          | UnOpAbs
  deriving (Show, Eq, Ord)

data BinOp = BinOpAdd
           | BinOpSub
           | BinOpMul
           | BinOpDiv
           | BinOpMod
           | BinOpPow
  deriving (Show, Eq, Ord)

data Expression = ExConst Int
                | ExVar String
                | ExUnary UnOp Expression
                | ExBinary BinOp Expression Expression
  deriving (Show)

type PartialParse a = (a, [Token])
type MaybeParse a = Maybe (PartialParse a)
type Parser a = [Token] -> MaybeParse a
type Expectation a = Token -> Maybe a
type BinOpExpectation = Expectation BinOp

expectAdditiveOp :: BinOpExpectation
expectAdditiveOp TokenPlus = Just BinOpAdd
expectAdditiveOp TokenMinus = Just BinOpSub
expectAdditiveOp _ = Nothing

expectMultiplicativeOp :: BinOpExpectation
expectMultiplicativeOp TokenAsterisk = Just BinOpMul
expectMultiplicativeOp TokenSlash = Just BinOpDiv
expectMultiplicativeOp TokenPercent = Just BinOpMod
expectMultiplicativeOp _ = Nothing

expectPowerOp :: BinOpExpectation
expectPowerOp TokenWedge = Just BinOpPow
expectPowerOp _ = Nothing

expectUnOp :: Expectation UnOp
expectUnOp TokenMinus = Just UnOpNegate
expectUnOp TokenAbs = Just UnOpAbs
expectUnOp _ = Nothing

expectInt :: Expectation Int
expectInt (TokenInt x) = Just x
expectInt _ = Nothing

expectIdent :: Expectation String
expectIdent (TokenIdent v) = Just v
expectIdent _ = Nothing

expectOpenPar :: Expectation ()
expectOpenPar (TokenOpenPar) = Just ()
expectOpenPar _ = Nothing

expectClosePar :: Expectation ()
expectClosePar (TokenClosePar) = Just ()
expectClosePar _ = Nothing

parseSingleToken :: (Token -> Maybe a) -> Parser a
parseSingleToken _ [] = Nothing
parseSingleToken f (t:ts) = fmap (,ts) $ f t

parseOpAndNextOperand :: BinOpExpectation -> Parser Expression -> Parser (BinOp, Expression)
parseOpAndNextOperand opf exf ts0 = do
  (op, ts1) <- parseSingleToken opf ts0
  (ex, ts2) <- exf ts1
  return ((op, ex), ts2)

parseBinOpSequence :: Parser Expression -> BinOpExpectation -> Parser Expression
parseBinOpSequence exf opf = (fmap $ parseBinOpSequence2 exf opf) . exf

parseBinOpSequence2 :: Parser Expression -> BinOpExpectation -> PartialParse Expression -> PartialParse Expression
parseBinOpSequence2 exf opf a@(ex1, ts) = (maybe a (parseBinOpSequence3 exf opf ex1)) $ parseOpAndNextOperand opf exf ts

parseBinOpSequence3 :: Parser Expression -> BinOpExpectation -> Expression -> PartialParse (BinOp, Expression) -> PartialParse Expression
parseBinOpSequence3 exf opf ex1 ((op, ex2), ts) = parseBinOpSequence2 exf opf (ExBinary op ex1 ex2, ts)

parseSum :: Parser Expression
parseSum = parseBinOpSequence parseProduct expectAdditiveOp

parseProduct :: Parser Expression
parseProduct = parseBinOpSequence parsePower expectMultiplicativeOp

parsePower :: Parser Expression
parsePower = parseBinOpSequence parseTerm expectPowerOp

parseAlternatives :: [Parser Expression] -> Parser Expression
parseAlternatives fs ts = join $ find isJust $ map (\f -> f ts) fs

parseTerm :: Parser Expression
parseTerm = parseAlternatives [parseUnaryOpAndBareTerm, parseBareTerm]

parseUnaryOpAndBareTerm :: Parser Expression
parseUnaryOpAndBareTerm ts0 = do
  (op, ts1) <- parseSingleToken expectUnOp ts0
  (ex, ts2) <- parseBareTerm ts1
  return (ExUnary op ex, ts2)

parseBareTerm :: Parser Expression
parseBareTerm = parseAlternatives [parseConst, parseVar, parseSubexpression]

parseConst :: Parser Expression
parseConst = (fmap (first ExConst)) . (parseSingleToken expectInt)

parseVar :: Parser Expression
parseVar = (fmap (first ExVar)) . (parseSingleToken expectIdent)

parseSubexpression ts0 = do
  (_, ts1) <- parseSingleToken expectOpenPar ts0
  (e, ts2) <- parseSum ts1
  (_, ts3) <- parseSingleToken expectClosePar ts2
  return (e, ts3)

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe p m@(Just x)
  | p x       = m
  | otherwise = Nothing

parse :: [Token] -> Maybe Expression
parse = fmap (fst) . filterMaybe (null . snd) . parseSum

unOpSemantics :: Map UnOp (Int -> Int)
unOpSemantics = fromList [
    (UnOpNegate, negate),
    (UnOpAbs, abs)
  ]

binOpSemantics :: Map BinOp (Int -> Int -> Int)
binOpSemantics = fromList [
    (BinOpAdd, (+)),
    (BinOpSub, (-)),
    (BinOpMul, (*)),
    (BinOpDiv, div),
    (BinOpMod, mod),
    (BinOpPow, (^))
  ]

eval :: Expression -> Map String Int -> Maybe Int
eval (ExConst x) _ = Just x
eval (ExVar v) m = M.lookup v m
eval (ExUnary op ex) m = (M.lookup op unOpSemantics) <*> (eval ex m)
eval (ExBinary op ex1 ex2) m = (M.lookup op binOpSemantics) <*> (eval ex1 m) <*> (eval ex2 m)

