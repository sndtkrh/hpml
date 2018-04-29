module PML.Command(Command(Q, Comment), Proved(Proved), run, cparse, emptyEnv) where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List as List
import PML.Formula
import qualified PML.Parser as Parser
import qualified PML.Rule as Rule
import qualified PML.Axiom as Axiom

data Indicator = INum Int | IStr String | IPre deriving (Show)

data Command
  = Axiom Formula
  | MP Indicator Indicator
  | US Indicator Formula String
  | G Indicator
  | Name Indicator String
  | Q
  | Comment String
  deriving (Show)

data Proved = Proved [Formula] (Map.Map String Formula) deriving (Show)
emptyEnv :: Proved
emptyEnv = Proved [] Map.empty

cparse :: String -> Maybe Command
cparse s = foldl f Nothing (cparse' s)
  where
    f :: Maybe Command -> Maybe Command -> Maybe Command
    f c d = case c of
      (Just c') -> return c'
      Nothing -> d

cparse' :: String -> [Maybe Command]
cparse' s = map
  (\ (prefix,f) -> case List.stripPrefix prefix s of
    Just s' -> f s'
    Nothing -> Nothing )
  [("Axiom",axiom), ("MP", mp), ("US", us), ("G", gn), ("Name", name), ("Q", quit), ("//", comment)]

axiom :: String -> Maybe Command
axiom s = case Parser.formula (Parser.skipSpaces s) of
  (Just (Parser.ParseState "" f)) -> Just (Axiom f)
  _ -> Nothing

mp :: String -> Maybe Command
mp s = case indicator s of
  (Just (i, s')) -> case indicator s' of
    (Just (j, "")) -> return $ MP i j
    _ -> Nothing
  Nothing -> Nothing

us :: String -> Maybe Command
us s = case indicator s of
  (Just (i, s')) -> case Parser.formula s' of
    (Just (Parser.ParseState s'' f)) -> case Parser.var s'' of
      ([], _) -> Nothing
      (v, "") -> return $ US i f v
      _ -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

gn :: String -> Maybe Command
gn s = case indicator s of
  (Just (i, "")) -> return $ G i
  _ -> Nothing

name :: String -> Maybe Command
name s = case indicator s of
  (Just (i, s')) -> let (v, s'') = Parser.var s' in
    if not (null v) && null s''
      then return $ Name i v
      else Nothing
  Nothing -> Nothing

quit :: String -> Maybe Command
quit s = if null $ Parser.skipSpaces s then return Q else Nothing

comment :: String -> Maybe Command
comment = return . Comment

indicator :: String -> Maybe (Indicator, String)
indicator (' ':s) = indicator s
indicator ('#':'^':s) = return (IPre, Parser.skipSpaces s)
indicator ('#':s@(c:_))
  | Char.isDigit c = let (n, s'') = readNum 0 s in return (INum n, Parser.skipSpaces s'')
  | Char.isAlpha c = let (var, s'') = readVar "" s in return (IStr var, Parser.skipSpaces s'')
  | otherwise = Nothing
indicator _ = Nothing

readNum :: Int -> String -> (Int, String)
readNum n s@(c:s')
  = if Char.isDigit c
    then readNum (n*10 + read [c]) s'
    else (n, s)
readNum n [] = (n, [])

readVar :: String -> String -> (String, String)
readVar v s@(c:s')
  = if Char.isAlpha c
    then readVar (v ++ [c]) s'
    else (v, s)
readVar v [] = (v, [])

run :: Command -> Proved -> Maybe Proved
run (Axiom f) (Proved thms m)
  = if Axiom.isAxiom f
    then return $ Proved (f:thms) m
    else Nothing
run (MP f fg) env@(Proved thms m)
  = case Rule.mp (getFormula f env) (getFormula fg env) of
    (Just g') -> return $ Proved (g':thms) m
    Nothing -> Nothing
run (G f) env@(Proved thms m)
  = case Rule.gn (getFormula f env) of
    (Just f') -> return $ Proved (f':thms) m
    Nothing -> Nothing
run (US f g' p) env@(Proved thms m)
  = case Rule.us (getFormula f env) (Just g') p of
    (Just f') -> return $ Proved (f':thms) m
    Nothing -> Nothing
run (Name f s) env@(Proved thms m)
  = case getFormula f env of
    (Just f') -> case Map.lookup s m of
      Nothing -> return $ Proved thms (Map.insert s f' m)
      (Just _) -> Nothing
    _ -> Nothing
run Q _ = Nothing
run (Comment _) _ = Nothing

getFormula :: Indicator -> Proved -> Maybe Formula
getFormula IPre (Proved (f:_) _) = return f
getFormula IPre (Proved [] _) = Nothing
getFormula (IStr key) (Proved _ m) = Map.lookup key m
getFormula (INum i) (Proved thms _)
  = if 0 <= i && i < length thms
    then return $ thms !! (length thms - i - 1)
    else Nothing
