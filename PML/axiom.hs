module PML.Axiom where

import PML.Formula

-- [](p->q)->([]p->[]q)
axiomK :: Formula
axiomK = Imply (Box (Imply (Var $ VariableIdentifier "p") (Var $ VariableIdentifier "q")))
          (Imply (Box (Var $ VariableIdentifier "p")) (Box (Var $ VariableIdentifier "q")) )
-- <>p<->~[]~p
axiomDualD :: Formula
axiomDualD = Equiv (Diamond (Var $ VariableIdentifier "p")) (Not (Box (Not (Var $ VariableIdentifier "p"))))
-- []p<->~<>~p
axiomDualB :: Formula
axiomDualB = Equiv (Box (Var $ VariableIdentifier "p")) (Not (Diamond (Not (Var $ VariableIdentifier "p"))))

axiom :: [Formula]
axiom = [axiomK, axiomDualB, axiomDualD]

isAxiom :: Formula -> Bool
isAxiom f = case isTautology f of
  Nothing -> f `elem` axiom
  Just True -> True
  Just False -> False
