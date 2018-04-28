module PML.Rule where

import PML.Formula

modusPonens :: Formula -> Formula -> Maybe Formula
modusPonens f (Imply f' g') = if f == f' then return g' else Nothing
modusPonens _ _ = Nothing

uniformSubstitute :: Formula -> Formula -> String -> Formula
uniformSubstitute f g p
  = case f of
    q@(Var s) -> if s == p then g else q
    Top -> Top
    Bottom -> Bottom
    (Not f') -> Not (uniformSubstitute f' g p)
    (Box f') -> Box (uniformSubstitute f' g p)
    (Diamond f') -> Diamond (uniformSubstitute f' g p)
    (Imply fl fr) -> Imply (uniformSubstitute fl g p) (uniformSubstitute fr g p)
    (Equiv fl fr) -> Equiv (uniformSubstitute fl g p) (uniformSubstitute fr g p)
    (And fl fr) -> And (uniformSubstitute fl g p) (uniformSubstitute fr g p)
    (Or fl fr) -> Or (uniformSubstitute fl g p) (uniformSubstitute fr g p)

generalize :: Formula -> Maybe Formula
generalize f = Just (Box f)

mp :: Maybe Formula -> Maybe Formula -> Maybe Formula
mp (Just f) (Just g) = modusPonens f g
mp _ _ = Nothing

gn :: Maybe Formula -> Maybe Formula
gn (Just f) = generalize f
gn _ = Nothing

us :: Maybe Formula -> Maybe Formula -> String -> Maybe Formula
us (Just f) (Just g) s = return $ uniformSubstitute f g s
us _ _ _ = Nothing
