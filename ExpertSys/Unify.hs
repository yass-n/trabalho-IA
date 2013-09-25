module ExpertSys.Unify where

import ExpertSys.Tipos

unify :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Maybe [Ligacao a]
unify Ign _ ligacoes = Just ligacoes

unify (Atomo p) (Seq _ _) _ = Nothing

unify (Atomo p) (Atomo d) ligacoes = if p == d then Just ligacoes else Nothing

unify (Variavel p) d ligacoes = unifyVariable (Variavel p) d ligacoes

unify p (Variavel d) ligacoes = unifyVariable (Variavel d) p ligacoes

unify (Seq firstP restP) (Seq firstD restD) ligacoes =
	unify firstP firstD ligacoes >>= unify restP restD

unifyVariable :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Maybe [Ligacao a]
unifyVariable (Variavel p) d ligacoes =
	case axaLigacao (Variavel p) ligacoes of
		Just (p, b) -> unify b d ligacoes
		Nothing -> if insideP (Variavel p) d ligacoes then Nothing else Just ls
    where ls = addLigacao (Variavel p) d ligacoes


insideP :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Bool
insideP v e ls = v /= e && insideOrEqualP v e ls

insideOrEqualP :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Bool
insideOrEqualP (Variavel v1) (Variavel v2) _ = v1 == v2

insideOrEqualP _ (Atomo e) _ = False

insideOrEqualP (Variavel v1) (Seq (Variavel v2) rest) ls =
    case axaLigacao (Variavel v2) ls of
        Just (_, b) -> insideOrEqualP (Variavel v1) b ls
        Nothing -> insideOrEqualP (Variavel v1) (Variavel v2) ls

insideOrEqualP v (Seq first rest) ls =
    insideOrEqualP v first ls || insideOrEqualP v rest ls
