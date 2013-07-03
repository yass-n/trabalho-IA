import Basics

--
unify :: Predicado -> Predicado -> Maybe [Ligacao]
unify (Predicado n1 t1) (Predicado n2 t2)
	| n1 == n2 = unifyTermos t1 t2 []
	| otherwise = Nothing

unifyTermos :: [Termo] -> [Termo] -> [Ligacao] -> Maybe [Ligacao]
unifyTermos [] [] ls = Just ls
unifyTermos