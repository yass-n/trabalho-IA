import Tipos
import Match
import Unify
import Stream
import Parser
import Expert
import Control.Monad.State

main:: IO ()
main = do
    putStrLn "teste do match"

    let x = Variavel "x"
    let y = Variavel "y"
    let underscore = Ign

    let apple = Atomo "apple"
    let red = Atomo "red"
    let orange = Atomo "orange"
    let color = Atomo "color"
    let person = Atomo "person"
    let is_a = Atomo "is-a"
    let with = Atomo "with"
    let hair = Atomo "hair"
    let patrick = Atomo "patrick"
    let blond = Atomo "blond"

    let p1 = Seq color (Seq x red)
    let p2 = Seq color (Seq apple red)
    let p3 = Seq color (Seq apple y)
    let p4 = Seq color (Seq x y)
    let p5 = Seq color (Seq underscore red)
    let p6 = Seq color (Seq underscore underscore)
    let p7 = Seq color (Seq apple orange)
    let p8 = Seq color (Seq x x)
    let p9 = Seq (Seq x (Seq is_a person)) (Seq with (Seq hair y))
    let p10 = Seq (Seq patrick (Seq is_a person)) (Seq with (Seq hair blond))

    let t1 = match p1 p2 [] == Just [(x, apple)]
    let t2 = match p3 p2 [] == Just [(y, red)]
    let t3 = match p4 p2 [] == Just [(y, red), (x, apple)]
    let t4 = match p2 p2 [] == Just []
    let t5 = match p5 p2 [] == Just []
    let t6 = match p6 p2 [] == Just []
    let t7 = match p7 p2 [] == Nothing
    let t8 = match p8 p2 [] == Nothing
    let t9 = match p9 p10 [] == Just [(y, blond), (x, patrick)]


    mapM_ testa [t1, t2, t3, t4, t5, t6, t7, t8, t9]

    putStrLn "teste do unify"

    let p11 =  Seq x (Seq with (Seq hair blond))
    let p12 = p10

    let p13 = Seq (Seq patrick (Seq is_a y)) (Seq with (Seq hair blond))
    let p14 = Seq (Seq patrick (Seq is_a x)) (Seq with (Seq hair blond))

    let t10 = unify p11 p12 [] == Just [(x, Seq patrick (Seq is_a person))]
    let t11 = unify p11 p13 [] == Just [(x, Seq patrick (Seq is_a y))]
    let t12 = unify p11 p14 [] == Nothing

    mapM_ testa [t10, t11, t12]

    putStrLn "teste do stream"

    -- Stream com três objectos ilustrado na pagina 368 do livro cap. 25
    let stream = Stream "object1" (Stream "object2" (Stream "object3" EmptyStream))

    -- Streams de exemplo nas paginas 369 e 370 criada com stream-cons
    let stream1 = streamCons "object_a" (streamCons "object_b" EmptyStream)
    let stream2 = streamCons "object_x" (streamCons "object_y" EmptyStream)
    let stream_of_streams = streamCons stream1 (streamCons stream2 EmptyStream)
    let number_stream = streamCons 2 (streamCons 3 EmptyStream)

    -- Teste das funcoes basicas
    let first = streamFirst stream
    let rest = streamRestp stream
    let isEnd = streamEndp (streamRestp (streamRestp rest))

    -- Teste stream-append ilustradas na pagina 369
    let streamA = streamAppend stream1 stream2

    -- Teste stream-concatenate ilustradas na pagina 369
    let streamB = streamConcatenate stream_of_streams

    -- Teste stream-transform ilustradas na pagina 370
    let streamC = streamTransform potenciaDeDois number_stream

    -- Teste stream-member
    let isMember = streamMember "object_x" streamB

    -- Teste stream-remember ilustradas na pagina 370
    let long_stream = streamB

    let aux = streamRemember "last_object" long_stream
    let long_stream = aux
    let status1 = long_stream

    let aux = streamRemember "last_object" long_stream
    let long_stream = aux
    let status2 = long_stream

    -- Testando
    let t13 = first == "object1"
    let t14 = rest == (Stream "object2" (Stream "object3" EmptyStream))
    let t15 = isEnd
    let t16 = streamA == (Stream "object_a" (Stream "object_b" (Stream "object_x"
                            (Stream "object_y" EmptyStream))))
    let t17 = stream_of_streams == (Stream (Stream "object_a" (Stream "object_b" EmptyStream))
                                        (Stream (Stream "object_x"
                                            (Stream "object_y" EmptyStream)) EmptyStream))
    let t18 = streamB == streamA
    let t19 = streamC == (Stream 4 (Stream 8 EmptyStream))
    let t20 = status1 == (Stream "object_a" (Stream "object_b" (Stream "object_x"
                            (Stream "object_y" (Stream "last_object" EmptyStream)))))
    let t21 = status2 == NIL

    mapM_ testa [t13, t14, t15, t16, t17, t18, t19]

    putStrLn "teste do parser"

    let (s1,_):_  = parse expr "color ?x red"
    let (s2,_):_  = parse expr "color apple red"
    let (s3,_):_  = parse expr "color apple ?y"
    let (s4,_):_  = parse expr "color ?x ?y"
    let (s5,_):_  = parse expr "color _ red"
    let (s6,_):_  = parse expr "color _ _"
    let (s7,_):_  = parse expr "color apple orange"
    let (s8,_):_  = parse expr "color ?x ?x"
    let (s9,_):_  = parse expr "(?x is-a person) with (hair ?y)"
    let (s10,_):_ = parse expr "(patrick is-a person) with (hair blond)"
    let (s11,_):_ = parse expr "?x with (hair blond)"
    let s12 = s10
    let (s13,_):_ = parse expr "(patrick is-a ?y) with (hair blond)"
    let (s14,_):_ = parse expr "(patrick is-a ?x) with (hair blond)"

    let t22 = s1 == p1
    let t23 = s2 == p2
    let t24 = s3 == p3
    let t25 = s4 == p4
    let t26 = s5 == p5
    let t27 = s6 == p6
    let t28 = s7 == p7
    let t29 = s8 == p8
    let t30 = s9 == p9

    let t31 = s10 == p10
    let t32 = s11 == p11
    let t33 = s12 == p12
    let t34 = s13 == p13
    let t35 = s14 == p14

    mapM_ testa [t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35]

    putStrLn "Teste do expert"


    let e str = ex where (ex,_):_ = parse expr str

    let animalIsASpecies = e "(? animal) is a (? species)"
    let animalIsAParent  = e "(? animal) is a parent of (? child)"
    let childIsASpecies  = e "(? child) is a (? species)"
    let deedeeIsAParent  = e "deedee is a parent of sugar"
    let deedeeIsAParent2 = e "deedee is a parent of brassy"
    let bozoIsADog = e "bozo is a dog"
    let deedeeIsAHorse = e "deedee is a horse"

    let try1 = tryAssertion animalIsASpecies bozoIsADog []

    let try2 = tryAssertion animalIsAParent deedeeIsAParent [(Variavel "species", Atomo "dog"), (Variavel "animal", Atomo "bozo")]

    let try3 = tryAssertion animalIsASpecies deedeeIsAHorse []

    let try4 = tryAssertion animalIsAParent deedeeIsAParent [(Variavel "species", Atomo "horse"), (Variavel "animal", Atomo "deedee")]


    let t36 = try1 == Stream [(Variavel "species", Atomo "dog"), (Variavel "animal", Atomo "bozo")] EmptyStream
    let t37 = try2 == EmptyStream
    let t38 = try3 == Stream [(Variavel "species", Atomo "horse"), (Variavel "animal", Atomo "deedee")] EmptyStream
    let t39 = try4 == Stream [(Variavel "child", Atomo "sugar"), (Variavel "species", Atomo "horse"), (Variavel "animal", Atomo "deedee")] EmptyStream

    let assertions = Stream bozoIsADog (Stream deedeeIsAHorse (Stream deedeeIsAParent (Stream deedeeIsAParent2 EmptyStream)))
    let rules = Stream (Rule "identify" [animalIsASpecies, animalIsAParent] childIsASpecies) EmptyStream
    let kb = Kb assertions rules

    let matchPTA1 = runState (matchPatternToAssertions animalIsASpecies []) kb
    let matchPTA2 = runState (matchPatternToAssertions animalIsAParent [(Variavel "species", Atomo "dog"), (Variavel "animal", Atomo "bozo")]) kb

    let t40 = matchPTA1 == (Stream [(Variavel "species", Atomo "dog"), (Variavel "animal", Atomo "bozo")] (Stream [(Variavel "species", Atomo "horse"), (Variavel "animal", Atomo "deedee")] EmptyStream), kb)
    let t41 = matchPTA2 == (EmptyStream, kb)

    mapM_ testa [t36, t37, t38, t39, t40, t41]

testa :: Bool -> IO ()
testa t = do
    if t then putStrLn " OK"
         else putStrLn " Fail"