module PML.StandardTranslation(modelST, frameST) where

import Data.Char as Char
import Data.Set as Set
import PML.Firstorder as Fo
import PML.Secondorder as So
import PML.Formula as Modal

modelST :: Modal.Formula -> Fo.Formula
modelST f = modelST' f (Modal.VariableIdentifier "x") 0

modelST' :: Modal.Formula -> Modal.VariableIdentifier -> Int -> Fo.Formula
modelST' (Modal.Var p) x n = Fo.MonadicPredicate p' x'
  where
    x' = variableTransrateMF x n
    p' = Fo.PredicateIdentifier $ Prelude.map Char.toUpper $ show p
modelST' Modal.Top _ _ = Fo.Top
modelST' Modal.Bottom _ _ = Fo.Bottom
modelST' (Modal.Not f) x n = Fo.Not $ modelST' f x n
modelST' (Modal.Box f) x n = Fo.Forall x'' $ Fo.Imply (Fo.BinRel Fo.basicrel x' x'') $ modelST' f x (n + 1)
  where
    x' = variableTransrateMF x n
    x'' = variableTransrateMF x (n + 1)
modelST' (Modal.Diamond f) x n = Fo.Exists x'' $ Fo.And (Fo.BinRel Fo.basicrel x' x'') $ modelST' f x (n + 1)
  where
    x' = variableTransrateMF x n
    x'' = variableTransrateMF x (n + 1)
modelST' (Modal.Imply f g) x n = Fo.Imply (modelST' f x n) $ modelST' g x n
modelST' (Modal.And f g) x n = Fo.And (modelST' f x n) $ modelST' g x n
modelST' (Modal.Or f g) x n = Fo.Or (modelST' f x n) $ modelST' g x n
modelST' (Modal.Equiv f g) x n = Fo.Equiv (modelST' f x n) $ modelST' g x n

variableTransrateMF :: Modal.VariableIdentifier -> Int -> Fo.VariableIdentifier
variableTransrateMF (Modal.VariableIdentifier s) n = Fo.VariableIdentifier $ s ++ show n



frameST :: Modal.Formula -> So.Formula
frameST f = Set.foldr So.Forall2 f' vs
  where
    f' = frameST' f (Modal.VariableIdentifier "x") 0
    vs = getSecondorderVariables f'

frameST' :: Modal.Formula -> Modal.VariableIdentifier -> Int -> So.Formula
frameST' (Modal.Var p) x n = So.Belongs p' x'
  where
    x' = variableTransrateMS x n
    p' = So.SecondorderVariableIdentifier $ Prelude.map Char.toUpper $ show p
frameST' Modal.Top _ _ = So.Top
frameST' Modal.Bottom _ _ = So.Bottom
frameST' (Modal.Not f) x n = So.Not $ frameST' f x n
frameST' (Modal.Box f) x n = So.Forall1 x'' $ So.Imply (So.BinRel So.basicrel x' x'') $ frameST' f x (n + 1)
  where
    x' = variableTransrateMS x n
    x'' = variableTransrateMS x (n + 1)
frameST' (Modal.Diamond f) x n = So.Exists1 x'' $ So.And (So.BinRel So.basicrel x' x'') $ frameST' f x (n + 1)
  where
    x' = variableTransrateMS x n
    x'' = variableTransrateMS x (n + 1)
frameST' (Modal.Imply f g) x n = So.Imply (frameST' f x n) $ frameST' g x n
frameST' (Modal.And f g) x n = So.And (frameST' f x n) $ frameST' g x n
frameST' (Modal.Or f g) x n = So.Or (frameST' f x n) $ frameST' g x n
frameST' (Modal.Equiv f g) x n = So.Equiv (frameST' f x n) $ frameST' g x n

variableTransrateMS :: Modal.VariableIdentifier -> Int -> So.FirstorderVariableIdentifier
variableTransrateMS (Modal.VariableIdentifier s) n = So.FirstorderVariableIdentifier $ s ++ show n

getSecondorderVariables :: So.Formula -> Set.Set So.SecondorderVariableIdentifier
getSecondorderVariables (So.Var1 _) = Set.empty
getSecondorderVariables (So.Var2 p) = Set.singleton p
getSecondorderVariables (So.Belongs p _) = Set.singleton p
getSecondorderVariables So.BinRel{} = Set.empty
getSecondorderVariables So.Top = Set.empty
getSecondorderVariables So.Bottom = Set.empty
getSecondorderVariables (So.Not f) = getSecondorderVariables f
getSecondorderVariables (So.And f g) = Set.union (getSecondorderVariables f) $ getSecondorderVariables g
getSecondorderVariables (So.Or f g) = Set.union (getSecondorderVariables f) $ getSecondorderVariables g
getSecondorderVariables (So.Imply f g) = Set.union (getSecondorderVariables f) $ getSecondorderVariables g
getSecondorderVariables (So.Equiv f g) = Set.union (getSecondorderVariables f) $ getSecondorderVariables g
getSecondorderVariables (So.Exists1 _ f) = getSecondorderVariables f
getSecondorderVariables (So.Forall1 _ f) = getSecondorderVariables f
getSecondorderVariables (So.Exists2 _ f) = getSecondorderVariables f
getSecondorderVariables (So.Forall2 _ f) = getSecondorderVariables f
