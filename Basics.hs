module Basics
( adicionaLigacao
, encontraLigacao
, Constante
, Variavel
, Ligacao
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
encontraLigacao v1 ((v2, c):t) = if v1 == v2 then Just (v2, c) else encontraLigacao v1 t