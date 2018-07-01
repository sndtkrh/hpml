module PML.Formula where

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype VariableIdentifier = VariableIdentifier String deriving (Show,Eq,Ord)
data Formula
  = Var VariableIdentifier
  | Top
  | Bottom
  | Not Formula
  | Box Formula
  | Diamond Formula
  | Imply Formula Formula
  | Equiv Formula Formula
  | And Formula Formula
  | Or Formula Formula
  deriving (Eq)

instance Show Formula where
  show (Var (VariableIdentifier s)) = s
  show Top = "T"
  show Bottom = "F"
  show (Not f) = '~' : show f
  show (Box f) = '[' : ']' : show f
  show (Diamond f) = '<' : '>' : show f
  show (Imply f g) = "(" ++ show f ++ "->" ++ show g ++ ")"
  show (Equiv f g) = "(" ++ show f ++ "<->" ++ show g ++ ")"
  show (And f g) = "(" ++ show f ++ "/\\" ++ show g ++ ")"
  show (Or f g) = "(" ++ show f ++ "\\/" ++ show g ++ ")"

isPropositionalFormula :: Formula -> Bool
isPropositionalFormula (Var _) = True
isPropositionalFormula Top = True
isPropositionalFormula Bottom = True
isPropositionalFormula (Not f) = isPropositionalFormula f
isPropositionalFormula (Box _) = False
isPropositionalFormula (Diamond _) = False
isPropositionalFormula (Imply f g)
  = isPropositionalFormula f && isPropositionalFormula g
isPropositionalFormula (Equiv f g)
  = isPropositionalFormula f && isPropositionalFormula g
isPropositionalFormula (And f g)
  = isPropositionalFormula f && isPropositionalFormula g
isPropositionalFormula (Or f g)
  = isPropositionalFormula f && isPropositionalFormula g

getPropositionalLetters :: Formula -> Set.Set VariableIdentifier
getPropositionalLetters f = case f of
  Var p -> Set.singleton p
  Top -> Set.empty
  Bottom -> Set.empty
  (Not g) -> getPropositionalLetters g
  (Box g) -> getPropositionalLetters g
  (Diamond g) -> getPropositionalLetters g
  (Imply g h) -> Set.union (getPropositionalLetters g) (getPropositionalLetters h)
  (Equiv g h) -> Set.union (getPropositionalLetters g) (getPropositionalLetters h)
  (And g h) -> Set.union (getPropositionalLetters g) (getPropositionalLetters h)
  (Or g h) -> Set.union (getPropositionalLetters g) (getPropositionalLetters h)

eval :: Formula -> Map.Map VariableIdentifier Bool -> Maybe Bool
eval f v = case f of
  (Var p) -> Map.lookup p v
  Top -> return True
  Bottom -> return False
  (Not g) -> do
    b <- eval g v
    return $ not b
  (Box _) -> Nothing
  (Diamond _) -> Nothing
  (Imply g h) -> do
    b <- eval g v
    c <- eval h v
    return $ not b || c
  (Equiv h g) -> do
    b <- eval g v
    c <- eval h v
    return $ b == c
  (And h g) -> do
    b <- eval g v
    c <- eval h v
    return $ b && c
  (Or h g) -> do
    b <- eval g v
    c <- eval h v
    return $ b || c

isTautology :: Formula -> Maybe Bool
isTautology f = foldl (\ a b -> (&&) <$> a <*> b) (pure True) $ map (isTautology' f) ls
  where
    ps = Set.toList $ getPropositionalLetters f
    n = length ps
    l = [(s,False) | s <- ps]
    ls' = l : map update ls'
    ls = take (2 ^ n) ls'

isTautology' :: Formula -> [(VariableIdentifier,Bool)] -> Maybe Bool
isTautology' f m = eval f $ Map.fromList m

update :: [(VariableIdentifier,Bool)] -> [(VariableIdentifier,Bool)]
update ((s,True):m) = (s,False) : update m
update ((s,False):m) = (s,True) : m
update [] = []
