module Expert where

import Control.Monad.State
import Tipos
import Match
import Stream

data Rule a = Rule { rName :: String
                   , rIfs  :: [Expressao a]
                   , rThen :: Expressao a
                   } deriving (Eq, Show)

-- | A base de conhecimento do sistema especialista. Stream de afirmações e
-- Stream de regras.

data Kb a = Kb { assertions :: ObjectStream (Expressao a)
               , rules      :: ObjectStream (Rule a)
               } deriving (Eq, Show)

-- | Tenta adicionar uma nova afirmação a base de conhecimento

rememberAssertion :: (Eq a) => Expressao a -> State (Kb a) Bool
rememberAssertion a = do
    Kb as rs <- get
    case streamRemember a as of
        EmptyStream -> return False
        as'         -> do { put $ Kb as' rs; return True }

-- | Tenta adicionar uma nova regra a base de conhecimento

rememberRule :: (Eq a) => Rule a -> State (Kb a) Bool
rememberRule r = do
    Kb as rs <- get
    case streamRemember r rs of
        -- indica que a regra não foi adicionada
        EmptyStream -> return False
        -- altera o estado, alterando Stream de Rules da Kb
        -- indica que a regra foi adicionada
        rs'         -> do { put $ Kb as rs'; return True }

-- | Tenta casar um padrão a uma afirmação dada uma lista de ligações / associações.
-- Se houver casamento retorna Stream com um elemento (a lista de associações
-- resultante), se não retorna EmptyStream.

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
-- Seta como resultado do State Stream de ligações resultantes do tryAssertion

matchPatternToAssertions :: (Eq a) =>
                            Expressao a
                            -> [Ligacao a] -> State (Kb a) (ObjectStream [Ligacao a])
matchPatternToAssertions p bs = do
    Kb as rs <- get
    return $ cat' $ map' (\a -> tryAssertion p a bs) as

-- | Aplica matchPatternToAssertions para cada associação da stream de associações
-- Como matchPatternToAssertions não modifica o estado, podemos usar o estado
-- passado para função

filterBindingStream :: (Eq a) =>
                       Expressao a
                       -> ObjectStream [Ligacao a]
                       -> State (Kb a) (ObjectStream [Ligacao a])
filterBindingStream p stream = do
    kb <- get
    return $ cat' $ map' (\bs -> evalState (matchPatternToAssertions p bs) kb) stream

-- | Aplica filterBindingStream a cada padrão numa lista de padrões

applyFilters :: (Eq a) =>
                [Expressao a]
                -> ObjectStream [Ligacao a] -> State (Kb a) (ObjectStream [Ligacao a])
applyFilters [] stream = return stream
applyFilters (h:t) stream = filterBindingStream h stream >>= applyFilters t

-- | Instancia a parte consequente de uma regra de acordo com a lista de associações

instantiateVariables :: (Eq a) => Expressao a -> [Ligacao a] -> Expressao a
instantiateVariables a@(Atomo p) _ = a

instantiateVariables v@(Variavel a) bs =
    case axaLigacao v bs of
        Just (x, y) -> y
      --Nothing -> pau!

instantiateVariables (Seq first rest) bs =
    Seq (instantiateVariables first bs) (instantiateVariables rest bs)


-- | Executa applyFilters em cada um dos antecedentes da regra, depois adiciona
-- os consequentes instanciados a stream de afirmações da base de conhecimento

useRule :: (Eq a, Show a) => Rule a -> StateT (Kb a) IO Bool
useRule r = do
    kb <- get
    let bindingStream = evalState (applyFilters (rIfs r) $ Stream [] EmptyStream) kb
    loop r bindingStream False
    where
      loop r (Stream b rest) switch = do
          let result = instantiateVariables (rThen r) b
          kb <- get
          let (bool, newKb) = runState (rememberAssertion result) kb
          if bool
              then do
                put newKb
                liftIO $ putStrLn $ "Rule " ++ rName r ++ " indicates " ++ show result
                loop r rest True
              else loop r rest switch
      loop r EmptyStream switch = return switch