module Match
( match
, Termo(..)
, Predicado(..)
) where

import Basics

type Lig = Ligacao Variavel Constante

-- Match os termos de dois predicados se tiverem o mesmo nome, caso contrário Nothing
match :: Predicado -> Predicado -> Maybe [Lig]
match (Predicado n1 t1) (Predicado n2 t2)
    | n1 == n2 = matchTermos t1 t2 []
    | otherwise = Nothing

-- Match os termos de dois predicados
matchTermos :: [Termo] -> [Termo] -> [Lig] -> Maybe [Lig]
matchTermos [] [] ls = Just ls
matchTermos ((Variavel v1):t1) (h2:t2) ls =                  -- O 1º elemento de 1ª lista é uma variável
    matchVariavel (Variavel v1) h2 ls >>= matchTermos t1 t2
matchTermos (h1:[]) (h2:[]) ls = matchAtomos h1 h2 ls        -- Cada lista de temos tem apenas um atomo
matchTermos (h1:t1) (h2:t2) ls =
    matchTermos [h1] [h2] ls >>= matchTermos t1 t2

matchAtomos :: Termo -> Termo -> [Lig] -> Maybe [Lig]
matchAtomos a1 a2 ls = if a1 == a2 then Just ls else Nothing

matchVariavel :: Termo -> Termo -> [Lig] -> Maybe [Lig]
matchVariavel (Variavel '_') _ ls = Just ls                  -- Ignora '_'
matchVariavel (Variavel v1) (Constante c1) ls =
    case encontraLigacao v1 ls of
        Just (Ligacao v2 c2) -> matchTermos [(Constante c2)] [(Constante c1)] ls
        Nothing -> Just (adicionaLigacao v1 c1 ls)