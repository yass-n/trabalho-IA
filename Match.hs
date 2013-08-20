module Match where

import Tipos

match :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> Maybe ([Ligacao a])
match Ign _ ligacoes = Just ligacoes

match (Atomo p) (Atomo d) ligacoes = if p == d then Just ligacoes else Nothing

match (Variavel _) (Seq _ _) _ = Nothing

match (Variavel p) d ligacoes =
    case axaLigacao (Variavel p) ligacoes of
        Just (p,b) -> match b d ligacoes
        Nothing -> Just (addLigacao (Variavel p) d ligacoes)

match (Seq firstP restP) (Seq firstD restD) ligacoes =
    match firstP firstD ligacoes >>=  match restP restD