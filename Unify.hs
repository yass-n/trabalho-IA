module Unify where

import Tipos

insideP :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Bool
insideP v e ls = if v == e then True else insideOrEqualP v e ls

insideOrEqualP :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Bool
insideOrEqualP (Variavel v1) (Variavel v2) _ = v1 == v2

insideOrEqualP _ (Atomo e) _ = False

insideOrEqualP (Variavel v1) (Seq (Variavel v2) rest) ls =
    case axaLigacao (Variavel v2) ls of
        Just (e1, e2) -> insideOrEqualP (Variavel v1) e2 ls
        Nothing -> insideOrEqualP (Variavel v1) (Variavel v2) ls

insideOrEqualP v (Seq first rest) ls =
    (insideOrEqualP v first ls) || (insideOrEqualP v rest ls)