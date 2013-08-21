module Expert where

import Control.Monad.State
import Tipos
import Match
import Stream

data Rule a = Rule { rName :: String
                   , rIfs  :: [Expressao a]
                   , rThen :: Expressao a
                   } deriving (Eq, Show)

-- | A base de conhecimento do sistema especialista. Uma Stream de afirmações e
-- | uma Stream de regras.

data Kb a = Kb { assertions :: ObjectStream (Expressao a)
               , rules      :: ObjectStream (Rule a)
               } deriving (Eq, Show)

-- | Tenta casar um padrão a uma afirmação dada uma lista de ligações/associações.
-- | Se houver casamento retorna Stream com um elemento (a lista de associações
-- | resultante), se não retorna EmptyStream.

tryAssertion :: (Eq a) =>
                Expressao a
                -> Expressao a -> [Ligacao a] -> ObjectStream [Ligacao a]
tryAssertion p a bs =
    case match p a bs of
        Nothing -> EmptyStream
        Just bs -> Stream bs EmptyStream

cat' = streamConcatenate
map' = streamTransform

-- | Casa um padrão com todas as afirmações na lista de afirmações do expert.
-- | Seta como resultado do State Stream de ligações resultantes do tryAssertion

matchPatternToAssertions :: (Eq a) =>
                            Expressao a
                            -> [Ligacao a] -> State (Kb a) (ObjectStream [Ligacao a])
matchPatternToAssertions p bs = do
    Kb as rs <- get
    return $ cat' $ map' (\a -> tryAssertion p a bs) as

-- | Aplica matchPatternToAssertions para cada associação da stream de associações
-- | Como matchPatternToAssertions não modifica o estado, podemos usar o estado
-- | passado para função

filterBindingStream :: (Eq a) =>
                       Expressao a
                       -> ObjectStream [Ligacao a]
                       -> State (Kb a) (ObjectStream [Ligacao a])
filterBindingStream p stream = do
    kb <- get
    return $ cat' $ map' (\bs -> evalState (matchPatternToAssertions p bs) kb) stream

-- | Aplica filterBindingStream a cada padrão numa lista de padrões

applyFilters :: (Eq a) =>
                ObjectStream (Expressao a)
                -> ObjectStream [Ligacao a] -> State (Kb a) (ObjectStream [Ligacao a])
applyFilters = undefined
