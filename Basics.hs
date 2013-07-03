module Basics
( adicionaLigacao
, encontraLigacao
, Constante
, Variavel
, Ligacao(..)
, Termo(..)
, Predicado(..)
) where

type Variavel = Char

type Constante = String

data Ligacao a b = Ligacao a b deriving (Eq)

-- Um termo pode ser uma variável ou uma constante
data Termo = Variavel Variavel | Constante Constante deriving (Eq, Show)

-- Um predicado é composto pelo nome (String) e uma lista de Termos
data Predicado = Predicado String [Termo] deriving (Show)

-- Adiciona uma ligação a lista de ligações.
adicionaLigacao :: (Eq a) =>  a -> b -> [Ligacao a b] -> [Ligacao a b]
adicionaLigacao a b ls = (Ligacao a b):ls

-- Encontra uma ligacao dada uma Variavel
encontraLigacao :: (Eq a) => a -> [Ligacao a b] -> Maybe (Ligacao a b)
encontraLigacao a [] = Nothing
encontraLigacao a ((Ligacao a2 b):t) = if a == a2 then Just (Ligacao a2 b) else encontraLigacao a t