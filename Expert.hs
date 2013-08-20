module Expert where

import Control.Monad.State
import Tipos
import Match
import Stream

data Rule a = Rule { rName :: String
                   , rIfs  :: [Expressao a]
                   , rThen :: Expressao a
                   } deriving (Show)

-- | A base de conhecimento do sistema especialista. Uma Stream de afirmações e
-- | uma Stream de regras.
data Kb a = Kb { assertions :: ObjectStream (Expressao a)
               , rules      :: ObjectStream (Rule a)
               } deriving (Show)

-- | Tenta casar um padrão a uma afirmação dada uma lista de ligações/associações.
-- | Se houver casamento retorna Stream com um elemento (a lista de associações
-- | resultante), se não retorna EmptyStream.
tryAssertion :: (Eq a) => Expressao a -> Expressao a -> [Ligacao a] -> ObjectStream [Ligacao a]
tryAssertion p a bs =
    case match p a bs of
        Nothing -> EmptyStream
        Just bs -> Stream bs EmptyStream

-- concatMapStream = streamConcatenate.streamTransform

-- | Casa um padrão com todas as afirmações na lista de afirmações do expert
matchPatternToAssertions :: (Eq a) => Expressao a -> [Ligacao a] -> State (Kb a) (ObjectStream [Ligacao a])
matchPatternToAssertions p bs = do
    Kb as rs <- get
    return $ streamConcatenate $ streamTransform (\a -> tryAssertion p a bs) as