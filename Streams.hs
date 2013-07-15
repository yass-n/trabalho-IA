module Stream where

data ObjectStream a = EmptyStream | Object a | Stream (ObjectStream a) (ObjectStream a) deriving (Show, Read, Eq)

{-
	EXEMPLOS DE USO:
	* Criando Streams:
		"Stream Vazia"			let x = EmptyStream
		"Stream com 1 obj"		let y = Stream (Object "object1") EmptyStream
		"Stream com 2 obj"		let z = Stream (Object "object1") (Stream (Object "object2") EmptyStream)
-}

streamEndp :: ObjectStream a -> Bool
streamEndp EmptyStream = True
streamEndp (Stream _ _) = False

streamFirst :: ObjectStream a -> ObjectStream a
streamFirst (Stream first _) = first

streamRestp :: ObjectStream a -> ObjectStream a
streamRestp (Stream _ rest) = rest

streamCons :: ObjectStream a -> ObjectStream a -> ObjectStream a
streamCons objeto stm = (Stream objeto stm)

streamAppend :: ObjectStream a -> ObjectStream a -> ObjectStream a
streamAppend stm1 stm2 =
	if (streamEndp stm1) then stm2 
	else streamCons (streamFirst stm1) (streamAppend (streamRestp stm1) stm2)

streamConcatenate :: ObjectStream a -> ObjectStream a
streamConcatenate streams
	| streamEndp streams = EmptyStream
	| streamEndp (streamFirst streams) = streamConcatenate (streamRestp streams)
	| otherwise = streamCons (streamFirst (streamFirst streams)) 
								(streamConcatenate (streamCons (streamRestp (streamFirst streams))
													(streamRestp streams)))

streamTranform :: (ObjectStream a -> ObjectStream a) -> ObjectStream a -> ObjectStream a
streamTranform procedure stream =
	if (streamEndp stream) then EmptyStream
	else streamCons (procedure (streamFirst stream))
						(streamTranform (procedure) (streamRestp stream))

streamMember :: (Eq a) => ObjectStream a -> ObjectStream a -> Bool
streamMember objeto stream
	| streamEndp stream = False
	| objeto == (streamFirst stream) = True
	| otherwise = streamMember objeto (streamRestp stream)

streamRemember :: ObjectStream a -> ObjectStream a -> ObjectStream a
streamRemember objeto variavel = undefined


{-------------------------------------------------------------------------------}

--Esta funcao foi criada para testar streamTranform
tira_primeira_letra :: ObjectStream String -> ObjectStream String
tira_primeira_letra (Object (h:t)) = (Object t)

--Funcao criada para testar streamTranform como exemplo do livro
potencia_de_dois :: ObjectStream Integer -> ObjectStream Integer
potencia_de_dois (Object num) = (Object (2^num))