module PML.Firstorder where

newtype VariableIdentifier = VariableIdentifier String deriving (Eq,Ord)
newtype PredicateIdentifier = PredicateIdentifier String deriving (Eq,Ord)
newtype RelationIdentifier = RelationIdentifier String deriving (Eq,Ord)
instance Show VariableIdentifier where
  show (VariableIdentifier s) = s
instance Show PredicateIdentifier where
  show (PredicateIdentifier s) = s
instance Show RelationIdentifier where
  show (RelationIdentifier s) = s

data Formula
  = Var VariableIdentifier
  | MonadicPredicate PredicateIdentifier VariableIdentifier
  | BinRel RelationIdentifier VariableIdentifier VariableIdentifier
  | Top
  | Bottom
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Imply Formula Formula
  | Equiv Formula Formula
  | Exists VariableIdentifier Formula
  | Forall VariableIdentifier Formula
  deriving (Eq)

instance Show Formula where
  show (Var x) = show x
  show (MonadicPredicate p x) = show p ++ show x
  show (BinRel r x y) = show r ++ show x ++ show y
  show Top = "⊤"
  show Bottom = "⊥"
  show (Not f) = "¬" ++ show f
  show (Imply f g) = "(" ++ show f ++ "→" ++ show g ++ ")"
  show (Equiv f g) = "(" ++ show f ++ "↔︎" ++ show g ++ ")"
  show (And f g) = "(" ++ show f ++ "∧" ++ show g ++ ")"
  show (Or f g) = "(" ++ show f ++ "∨" ++ show g ++ ")"
  show (Exists x f) = "∃" ++ show x ++ "(" ++ show f ++ ")"
  show (Forall x f) = "∀" ++ show x ++ "(" ++ show f ++ ")"

basicrel :: RelationIdentifier
basicrel = RelationIdentifier "R"
