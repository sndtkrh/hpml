module PML.StandardTranslation(modelST) where

import Data.Char
import PML.Firstorder as Fo
-- import PML.Secondorder as So
import PML.Formula as Modal

modelST :: Modal.Formula -> Fo.Formula
modelST f = modelST' f (Modal.VariableIdentifier "x") 0

modelST' :: Modal.Formula -> Modal.VariableIdentifier -> Int -> Fo.Formula
modelST' (Modal.Var p) x n = Fo.MonadicPredicate p' x'
  where
    x' = variableTransrateMF x n
    p' = Fo.PredicateIdentifier $ map toUpper $ show p
modelST' Modal.Top _ _ = Fo.Top
modelST' Modal.Bottom _ _ = Fo.Bottom
modelST' (Modal.Not f) x n = Fo.Not $ modelST' f x n
modelST' (Modal.Box f) x n = Fo.Forall x'' $ Fo.Imply (Fo.BinRel basicrel x' x'') $ modelST' f x (n + 1)
  where
    x' = variableTransrateMF x n
    x'' = variableTransrateMF x (n + 1)
modelST' (Modal.Diamond f) x n = Fo.Exists x'' $ Fo.And (Fo.BinRel basicrel x' x'') $ modelST' f x (n + 1)
  where
    x' = variableTransrateMF x n
    x'' = variableTransrateMF x (n + 1)
modelST' (Modal.Imply f g) x n = Fo.Imply (modelST' f x n) $ modelST' g x n
modelST' (Modal.And f g) x n = Fo.And (modelST' f x n) $ modelST' g x n
modelST' (Modal.Or f g) x n = Fo.Or (modelST' f x n) $ modelST' g x n
modelST' (Modal.Equiv f g) x n = Fo.Equiv (modelST' f x n) $ modelST' g x n

variableTransrateMF :: Modal.VariableIdentifier -> Int -> Fo.VariableIdentifier
variableTransrateMF (Modal.VariableIdentifier s) n = Fo.VariableIdentifier $ s ++ show n

basicrel :: Fo.RelationIdentifier
basicrel = Fo.RelationIdentifier "R"
