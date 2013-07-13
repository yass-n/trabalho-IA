module Unify where

import Tipos

unify :: Expression String -> Expression String -> [Ligacao String] -> Maybe ([Ligacao String])
unify (Variavel "_") _ ligacoes = Just ligacoes

unify (Atomo p) (Atomo d) ligacoes = if p == d then Just ligacoes else Nothing

unify (Variavel p) d ligacoes = unify_variable p d ligacoes

unify p (Variavel d) ligacoes = unify_variable d p ligacoes

unify (Seq firstP restP) (Seq firstD restD) ligacoes = 
	unify firstP firstD ligacoes >>= unify restP restD


unify_variable :: Expression String -> Expression String -> [Ligacao String] -> Maybe ([Ligacao String])
unify_variable (Variavel p) d ligacoes = 
	case axaLigacao (Variavel p) ligacoes of
		Just (p,b) -> unify b d ligacoes
		Nothing -> if insidep p d ligacoes then Nothing else Just (addLigacao (Variavel p) d ligacoes)