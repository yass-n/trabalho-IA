module Tipos
( axaLigacao
, addLigacao
, Ligacao
, Expressao(..)
) where

data Expressao a = Atomo a | Variavel a | Seq (Expressao a) (Expressao a) | Ign deriving (Eq, Read)

type Ligacao a = (Expressao a, Expressao a)

-- Adiciona uma ligação a lista de ligações.
addLigacao :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> [Ligacao a]
addLigacao a b ls = (a, b):ls

-- Encontra uma ligacao dada uma Variavel
axaLigacao :: (Eq a) => Expressao a -> [Ligacao a] -> Maybe (Ligacao a)
axaLigacao a [] = Nothing
axaLigacao a ((b,c):t) = if a == b then Just (b,c) else axaLigacao a t

instance (Show a) => Show (Expressao a) where
    show Ign = "_"
    show (Variavel a) = '?' : show a
    show (Atomo a) = show a
    show (Seq e es) = show e ++ " " ++ show es