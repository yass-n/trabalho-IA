module Stream where

data ObjectStream a = EmptyStream | Stream a (ObjectStream a) deriving (Show, Read, Eq)

stream_endp :: ObjectStream a -> Bool
stream_endp EmptyStream = True
stream_endp (Stream _ _) = False

stream_first :: ObjectStream a -> ObjectStream a
stream_first (Stream first _) = first

--stream_rest :: ObjectStream a -> ObjectStream a
--stream_rest (Stream _ rest) = rest

--stream_cons :: ObjectStream a -> ObjectStream a -> ObjectStream a
--stream_cons objeto stm = (Stream objeto stm)

--stream_append :: ObjectStream a -> ObjectStream a -> ObjectStream a
--stream_append stm1 stm2 =
--	if (stream_endp stm1) then stm2 
--	else stream_cons (stream_first stm1) (stream_append (stream_rest stm1) stm2)