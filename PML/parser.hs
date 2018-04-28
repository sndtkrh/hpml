module PML.Parser(ParseState(ParseState), parse, formula, var, skipSpaces) where

import Data.Char
import PML.Formula

data ParseState = ParseState String Formula deriving (Show)

parse :: String -> Maybe Formula
parse s = do
  (ParseState r f) <- formula s
  if null r
    then return f
    else Nothing

formula :: String -> Maybe ParseState
formula s = do
  (ParseState s' f) <- subformula (skipSpaces s)
  if null s'
    then return $ ParseState "" f
    else case s' of
      ('-':'>':t) -> do
        (ParseState s'' g) <- formula t
        return $ ParseState (skipSpaces s'') $ Imply f g
      ('/':'\\':t) -> do
        (ParseState s'' g) <- formula t
        return $ ParseState (skipSpaces s'') $ And f g
      ('\\':'/':t) -> do
        (ParseState s'' g) <- formula t
        return $ ParseState (skipSpaces s'') $ Or f g
      ('<':'-':'>':t) -> do
        (ParseState s'' g) <- formula t
        return $ ParseState (skipSpaces s'') $ Equiv f g
      _ -> return $ ParseState (skipSpaces s') f

subformula :: String -> Maybe ParseState
subformula ('(':s) = do
  (ParseState s' f) <- formula s
  case s' of
    (')':s'') -> return $ ParseState s'' f
    _ -> Nothing
subformula ('~':s) = do
  (ParseState s' f) <- subformula s
  return $ ParseState s' $ Not f
subformula ('[':']':s) = do
  (ParseState s' f) <- subformula s
  return $ ParseState s' $ Box f
subformula ('<':'>':s) = do
  (ParseState s' f) <- subformula s
  return $ ParseState s' $ Diamond f
subformula s = let (varname, s') = var s in
  case varname of
    [] -> Nothing
    "T" -> return $ ParseState s' Top
    "F" -> return $ ParseState s' Bottom
    _ -> return $ ParseState s' $ Var varname

var :: String -> (String, String)
var = var' ""
var' :: String -> String -> (String, String)
var' t s@(c:s') = if isAlpha c
  then var' (t ++ [c]) s'
  else (t, skipSpaces s)
var' t [] = (t, [])

skipSpaces :: String -> String
skipSpaces = dropWhile ( == ' ')
