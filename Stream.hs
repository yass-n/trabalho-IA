module Stream where

data ObjectStream a = EmptyStream | Stream a (ObjectStream a) | NIL deriving (Show, Read, Eq)

{-
	EXEMPLOS DE USO:
	* Criando Streams:
		"Stream Vazia"			let x = EmptyStream
		"Stream com 1 obj"		let y = Stream "object1" EmptyStream
		"Stream com 2 obj"		let z = Stream "object1" (Stream "object2" EmptyStream)
-}

streamEndp :: ObjectStream a -> Bool
streamEndp EmptyStream = True
streamEndp (Stream _ _) = False

streamFirst :: ObjectStream a -> a
streamFirst (Stream object _) = object

streamRestp :: ObjectStream a -> ObjectStream a
streamRestp (Stream _ rest) = rest

streamCons :: a -> ObjectStream a -> ObjectStream a
streamCons = Stream

streamAppend :: ObjectStream a -> ObjectStream a -> ObjectStream a
streamAppend stm1 stm2 =
	if (streamEndp stm1) then stm2
	else streamCons (streamFirst stm1) (streamAppend (streamRestp stm1) stm2)

streamConcatenate :: ObjectStream (ObjectStream a) -> ObjectStream a
streamConcatenate streams
	| streamEndp streams = EmptyStream
	| streamEndp (streamFirst streams) = streamConcatenate (streamRestp streams)
	| otherwise = streamCons (streamFirst (streamFirst streams))
								(streamConcatenate (streamCons (streamRestp (streamFirst streams))
													(streamRestp streams)))

streamTransform :: (a -> b) -> ObjectStream a -> ObjectStream b
streamTransform procedure stream =
	if (streamEndp stream) then EmptyStream
	else streamCons (procedure (streamFirst stream))
						(streamTransform (procedure) (streamRestp stream))

streamMember :: (Eq a) => a -> ObjectStream a -> Bool
streamMember objeto stream
	| streamEndp stream = False
	| objeto == (streamFirst stream) = True
	| otherwise = streamMember objeto (streamRestp stream)

streamRemember :: (Eq a) => a -> ObjectStream a -> ObjectStream a
streamRemember objeto stream =
	if not (streamMember objeto stream) then streamAppend stream (streamCons objeto EmptyStream)
	else EmptyStream


{-------------------------------------------------------------------------------}

--Esta funcao foi criada para testar streamTransform
tiraPrimeiraLetra :: String -> String
tiraPrimeiraLetra = tail

--Funcao criada para testar streamTransform como exemplo do livro
potenciaDeDois :: Integer -> Integer
potenciaDeDois = (2^)