{- the monadic second order language -}
module PML.Secondorder where

newtype FirstorderVariableIdentifier = FirstorderVariableIdentifier String deriving (Eq,Ord)
newtype SecondorderVariableIdentifier = SecondorderVariableIdentifier String deriving (Eq,Ord)
newtype RelationIdentifier = RelationIdentifier String deriving (Eq,Ord)
instance Show FirstorderVariableIdentifier where
  show (FirstorderVariableIdentifier s) = s
instance Show SecondorderVariableIdentifier where
  show (SecondorderVariableIdentifier s) = s
instance Show RelationIdentifier where
  show (RelationIdentifier s) = s

data Formula
  = Var1 FirstorderVariableIdentifier
  | Var2 SecondorderVariableIdentifier
  | Belongs SecondorderVariableIdentifier FirstorderVariableIdentifier
  | BinRel RelationIdentifier FirstorderVariableIdentifier FirstorderVariableIdentifier
  | Top
  | Bottom
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imply Formula Formula
  | Equiv Formula Formula
  | Exists1 FirstorderVariableIdentifier Formula
  | Forall1 FirstorderVariableIdentifier Formula
  | Exists2 SecondorderVariableIdentifier Formula
  | Forall2 SecondorderVariableIdentifier Formula
  deriving (Eq)

instance Show Formula where
  show (Var1 x) = show x
  show (Var2 p) = show p
  show (Belongs p x) = show p ++ show x
  show (BinRel r x y) = show r ++ show x ++ show y
  show Top = "⊤"
  show Bottom = "⊥"
  show (Not f) = "¬" ++ show f
  show (Imply f g) = "(" ++ show f ++ "→" ++ show g ++ ")"
  show (Equiv f g) = "(" ++ show f ++ "↔︎" ++ show g ++ ")"
  show (And f g) = "(" ++ show f ++ "∧" ++ show g ++ ")"
  show (Or f g) = "(" ++ show f ++ "∨" ++ show g ++ ")"
  show (Exists1 x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
  show (Forall1 x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"
  show (Exists2 x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
  show (Forall2 x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"

basicrel :: RelationIdentifier
basicrel = RelationIdentifier "R"
