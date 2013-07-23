module Stream where

data ObjectStream a = EmptyStream | Object a | Stream (ObjectStream a) (ObjectStream a) | NIL deriving (Show, Read, Eq)

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

streamTransform :: (ObjectStream a -> ObjectStream a) -> ObjectStream a -> ObjectStream a
streamTransform procedure stream =
	if (streamEndp stream) then EmptyStream
	else streamCons (procedure (streamFirst stream))
						(streamTransform (procedure) (streamRestp stream))

streamMember :: (Eq a) => ObjectStream a -> ObjectStream a -> Bool
streamMember objeto stream
	| streamEndp stream = False
	| objeto == (streamFirst stream) = True
	| otherwise = streamMember objeto (streamRestp stream)

streamRemember :: (Eq a) => ObjectStream a -> ObjectStream a -> ObjectStream a
streamRemember objeto variavel = 
	if not (streamMember objeto variavel) then streamAppend variavel (streamCons objeto EmptyStream)
	else NIL


{-------------------------------------------------------------------------------}

--Esta funcao foi criada para testar streamTransform
tiraPrimeiraLetra :: ObjectStream String -> ObjectStream String
tiraPrimeiraLetra (Object (h:t)) = (Object t)

--Funcao criada para testar streamTransform como exemplo do livro
potenciaDeDois :: ObjectStream Integer -> ObjectStream Integer
potenciaDeDois (Object num) = (Object (2^num))