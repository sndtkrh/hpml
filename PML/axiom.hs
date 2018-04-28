module PML.Axiom where

import PML.Formula

-- [](p->q)->([]p->[]q)
axiomK :: Formula
axiomK = Imply (Box (Imply (Var "p") (Var "q"))) (Imply (Box (Var "p")) (Box (Var "q")) )

-- <>p<->~[]~p
axiomDualD :: Formula
axiomDualD = Equiv (Diamond (Var "p")) (Not (Box (Not (Var "p"))))
-- []p<->~<>~p
axiomDualB :: Formula
axiomDualB = Equiv (Box (Var "p")) (Not (Diamond (Not (Var "p"))))

axiom :: [Formula]
axiom = [axiomK, axiomDualB, axiomDualD]

isAxiom :: Formula -> Bool
isAxiom f = case isTautology f of
  Nothing -> f `elem` axiom
  Just True -> True
  Just False -> False
