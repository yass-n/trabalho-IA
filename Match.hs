module Match
( match
, Termo(..)
, Predicado(..)
) where

type Variavel = Char

type Constante = String

type Ligacao = (Variavel, Constante)

-- Um termo pode ser uma variável ou uma constante
data Termo = Variavel Variavel | Constante Constante deriving (Eq, Show)

-- Um predicado é composto pelo nome (String) e uma lista de Termos
data Predicado = Predicado String [Termo] deriving (Show)

-- Adiciona uma ligação a lista de ligações. A variável '_' não casa com nenhuma
-- constante, logo devem ser ignoradas as ligações com '_'
adicionaLigacao :: Variavel -> Constante -> [Ligacao] -> [Ligacao]
adicionaLigacao '_' _ ls = ls
adicionaLigacao v c [] = [(v,c)]
adicionaLigacao v c ls = (v,c):ls

-- Encontra uma ligacao dada uma Variavel
encontraLigacao :: Variavel -> [Ligacao] -> Maybe Ligacao
encontraLigacao v [] = Nothing
encontraLigacao '_' _ = Nothing
encontraLigacao v1 ((v2, c):t)
	| v1 == v2 = Just (v2, c)
	| otherwise = encontraLigacao v1 t

-- Match os termos de dois predicados se tiverem o mesmo nome, caso contrário Nothing
match :: Predicado -> Predicado -> Maybe [Ligacao]
match (Predicado n1 t1) (Predicado n2 t2)
    | n1 == n2 = matchTermos t1 t2 []
    | otherwise = Nothing

-- Match os termos de dois predicados
matchTermos :: [Termo] -> [Termo] -> [Ligacao] -> Maybe [Ligacao]
matchTermos [] [] ls = Just ls
matchTermos ((Variavel v1):t1) (h2:t2) ls =                                  -- O 1º elemento de 1ª lista é uma variável
    matchVariavel (Variavel v1) h2 ls >>= (\ls2 -> matchTermos t1 t2 ls2)
matchTermos (h1:[]) (h2:[]) ls = matchAtomos h1 h2 ls                        -- Cada lista de temos tem apenas um atomo
matchTermos (h1:t1) (h2:t2) ls =
    matchTermos [h1] [h2] ls >>= (\ls2 -> matchTermos t1 t2 ls2)

matchAtomos :: Termo -> Termo -> [Ligacao] -> Maybe [Ligacao]
matchAtomos a1 a2 ls
    | a1 == a2 = Just ls
    | otherwise  = Nothing

matchVariavel :: Termo -> Termo -> [Ligacao] -> Maybe [Ligacao]
matchVariavel (Variavel v1) (Constante c1) ls =
    case ligacao of Just (v2, c2) -> matchTermos [(Constante c2)] [(Constante c1)] ls
                    Nothing -> Just (adicionaLigacao v1 c1 ls)
    where ligacao = encontraLigacao v1 ls
