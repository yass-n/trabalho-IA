module Parser where

import Tipos
import Control.Monad
import Data.Char

-- Um parser do tipo a é uma função que recebe uma string e retorna uma lista
-- de resultados
-- Uma lista vazia significa que o parse não teve sucesso. No caso de sucesso
-- cada resultado é uma 2-tupla onde o primeiro elemento é o resultado do parse
-- (valor do tipo a) de um prefixo da string e o segundo elemento o sufixo da string.
-- Para gramáticas ambíguas serão retornados vários resultados se o prefixo puder
-- ser consumido de diferentes maneiras
newtype Parser a = Parser (String -> [(a, String)])

-- A função parse recebe um parser do tipo a e o retorna em forma de função
-- (extrai a função do parser)
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

-- Um Monad é uma abstração que representa uma computação.
-- Parsers podem ser especificados como monads, para isso eles devem ser uma
-- instância da classe Monad.
-- O parser return executa com sucesso sem consumir nenhum prefixo da string de entrada.
-- (>>=) é um operador para executar parsers em sequência, primeiro é executado p1
-- na string de entrada o que resulta numa lista de tuplas, em seguida executa-se
-- p2 sobre cada um dos elementos do resultado anterior e no final os resultados
-- são concatenados para formar uma única lista de resultados.
instance Monad Parser where
    return a  = Parser (\cs -> [(a, cs)])
    p1 >>= p2 = Parser (\cs -> concat [parse (p2 a) cs' | (a, cs') <- parse p1 cs])

-- MonadPlus permite representar uma computação que não teve sucesso (mzero) e
-- juntar duas computações (mplus)
instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    p1 `mplus` p2 = Parser (\cs -> parse p1 cs ++ parse p2 cs)

-- O operador (+++) retorna o resultado do primeiro parser se não houver erros,
-- caso contrário retorna o resultado do segundo parser
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser (\cs -> case parse (p1 `mplus` p2) cs of
                            [] -> []
                            (x:xs)-> [x])

-- O parser item consome o primeiro caractere da string de entrada e o
-- retorna como resultado, se a string for vazia o parser falha (retorna [])
item :: Parser Char
item =  Parser (\cs -> case cs of
                        "" -> []
                        (c:cs) -> [(c, cs)])

-- Semelhante ao item, executa com sucesso se o caractere extraído satisfaz
-- o predicado p
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

-- Parser para um caractere especifico
char :: Char -> Parser Char
char c = sat (c ==)

-- Parser para uma string especifica
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

-- many executa o parser p zero ou mais vezes, many1 executa o parser p uma ou
-- mais vezes
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- String de espaços, tabs, e newLines
space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

-- Um atomo é uma sequencia de letras e numeros e o separador '-'
tokAtom :: Parser String
tokAtom = token $ many $ sat (\c -> c `elem` '-':['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])

{-
   Gramática
   =========
   expr := "(" expr ")" expr | atom expr | variavel expr | ign expr | vazio
   atom := [A-Za-z0-9] ++ ['-']
   var  := "?" idf
   idf  := a | b | ... | z
   ign  := "_"
-}

expr :: Parser (Expressao String)
expr =  do { symb "(";  e <- expr; symb ")"; es <- expr; return (Seq e es) }
    +++ do { a <- atom; e <- expr;                       return (Seq a e) }
    +++ do { v <- var;  e <- expr;                       return (Seq v e) }
    +++ do { i <- ign;  e <- expr;                       return (Seq i e) }
    +++ do { symb "(";  e <- expr; symb ")";             return e }
    +++ atom
    +++ var
    +++ ign

atom :: Parser (Expressao String)
atom = do {a <- tokAtom; if a == "" then mzero else return (Atomo a)}

var :: Parser (Expressao String)
var = do {symb "?"; v <- tokAtom;                   return (Variavel v)}

ign :: Parser (Expressao String)
ign = do {token $ string "_";                       return Ign}