module PML.Formulaproperties where
import Data.Set as S
import PML.Formula

isVerySimpleShalqvist :: Formula -> Bool
isVerySimpleShalqvist (Imply antecedent consequent)
  = isVerySimpleShalqvistAntecedent antecedent && isPositive consequent
isVerySimpleShalqvist _ = False

isVerySimpleShalqvistAntecedent :: Formula -> Bool
isVerySimpleShalqvistAntecedent (Var _) = True
isVerySimpleShalqvistAntecedent Top = True
isVerySimpleShalqvistAntecedent Bottom = True
isVerySimpleShalqvistAntecedent (Diamond f) = isVerySimpleShalqvistAntecedent f
isVerySimpleShalqvistAntecedent (And f g) =
  isVerySimpleShalqvistAntecedent f && isVerySimpleShalqvistAntecedent g
isVerySimpleShalqvistAntecedent _ = False

isPositive :: Formula -> Bool
isPositive f = S.foldl (\ b p -> b && isPositive' f p) True s
  where s = getPropositionalLetters f

isNegative :: Formula -> Bool
isNegative f = S.foldl (\ b p -> b && isNegative' f p) True s
  where s = getPropositionalLetters f

isPositive' :: Formula -> VariableIdentifier -> Bool
isPositive' (Var _) _ = True
isPositive' Top _ = True
isPositive' Bottom _ = True
isPositive' (Not f) p = isNegative' f p
isPositive' (Diamond f) p = isPositive' f p
isPositive' (Box f) p = isPositive' f p
isPositive' (Imply f g) p = isNegative' f p && isPositive' g p
isPositive' (Equiv f g) p = isPositive' (Imply f g) p && isPositive' (Imply g f) p
isPositive' (And f g) p = isPositive' f p && isPositive' g p
isPositive' (Or f g) p = isPositive' f p && isPositive' g p

isNegative' :: Formula -> VariableIdentifier -> Bool
isNegative' (Var q) p = q /= p
isNegative' Top _ = True
isNegative' Bottom _ = True
isNegative' (Not f) p = isPositive' f p
isNegative' (Diamond f) p = isNegative' f p
isNegative' (Box f) p = isNegative' f p
isNegative' (Imply f g) p = isPositive' f p && isNegative' g p
isNegative' (Equiv f g) p = isNegative' (Imply f g) p && isNegative' (Imply g f) p
isNegative' (And f g) p = isNegative' f p && isNegative' g p
isNegative' (Or f g) p = isNegative' f p && isNegative' g p
