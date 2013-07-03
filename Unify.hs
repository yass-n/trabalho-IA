import Basics

type Lig2 = Ligacao Variavel Termo


-- Unifica dois predicados compativeis e retorna Nothing para os incompativeis
unify :: Predicado -> Predicado -> Maybe [Lig2]
unify (Predicado n1 t1) (Predicado n2 t2)
    | n1 == n2 = unifyTermos t1 t2 []
    | otherwise = Nothing

-- Unifica os termos dos predicados
unifyTermos :: [Termo] -> [Termo] -> [Lig2] -> Maybe [Lig2]
unifyTermos [] [] ls = Just ls
unifyTermos ((Variavel v1):t1) (h2:t2) ls =
    unifyVariavel (Variavel v1) h2 ls >>= unifyTermos t1 t2
unifyTermos (h1:t1) ((Variavel v2):t2) ls =
    unifyVariavel (Variavel v2) h1 ls >>= unifyTermos t1 t2
unifyTermos (h1:[]) (h2:[]) ls = unifyAtomos h1 h2 ls
unifyTermos (h1:t1) (h2:t2) ls=
    unifyTermos [h1] [h2] ls >>= unifyTermos t1 t2

-- Recebe dois Atomos e compara se sao iguais, unifica se sim
unifyAtomos :: Termo -> Termo -> [Lig2] -> Maybe [Lig2]
unifyAtomos a1 a2 ls = if a1 == a2 then Just ls else Nothing

unifyVariavel :: Termo -> Termo -> [Lig2] -> Maybe [Lig2]
unifyVariavel (Variavel '_') _ ls = Just ls
unifyVariavel (Variavel v1) c1 ls =
    case encontraLigacao v1 ls of
        Just (Ligacao v2 c2) -> unifyTermos [c2] [c1] ls
        Nothing -> dentro (Variavel v1) c1 ls


dentro :: Termo -> Termo -> [Lig2] -> Maybe [Lig2]
dentro v1 v2 ls
    | v1 == v2 = Nothing
    | otherwise = Nothing