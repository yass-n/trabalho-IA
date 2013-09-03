import Tipos
import Match
import Unify
import Stream
import Parser
import Expert
import Control.Monad.State
import Data.Maybe
import System.Exit

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
    let t7 = isNothing $ match p7 p2 []
    let t8 = isNothing $ match p8 p2 []
    let t9 = match p9 p10 [] == Just [(y, blond), (x, patrick)]


    mapM_ testa [t1, t2, t3, t4, t5, t6, t7, t8, t9]

    putStrLn "teste do unify"

    let p11 =  Seq x (Seq with (Seq hair blond))
    let p12 = p10

    let p13 = Seq (Seq patrick (Seq is_a y)) (Seq with (Seq hair blond))
    let p14 = Seq (Seq patrick (Seq is_a x)) (Seq with (Seq hair blond))

    let t10 = unify p11 p12 [] == Just [(x, Seq patrick (Seq is_a person))]
    let t11 = unify p11 p13 [] == Just [(x, Seq patrick (Seq is_a y))]
    let t12 = isNothing $ unify p11 p14 []

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
    let t14 = rest == Stream "object2" (Stream "object3" EmptyStream)
    let t15 = isEnd
    let t16 = streamA == Stream "object_a"
                            (Stream "object_b"
                                (Stream "object_x"
                                    (Stream "object_y" EmptyStream)))

    let t17 = stream_of_streams == Stream
                                        (Stream "object_a"
                                            (Stream "object_b" EmptyStream))
                                        (Stream (Stream "object_x"
                                            (Stream "object_y" EmptyStream))
                                        EmptyStream)
    let t18 = streamB == streamA
    let t19 = streamC == Stream 4 (Stream 8 EmptyStream)
    let t20 = status1 == Stream "object_a"
                            (Stream "object_b"
                                (Stream "object_x"
                                    (Stream "object_y"
                                        (Stream "last_object" EmptyStream))))
    let t21 = status2 == EmptyStream

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

    --fn :: Parser a -> a -> Expressao a

    let animalIsASpecies = e "(? animal) is a (? species)"
    let animalIsAParent  = e "(? animal) is a parent of (? child)"
    let childIsASpecies  = e "(? child) is a (? species)"
    let deedeeIsAParent  = e "deedee is a parent of sugar"
    let deedeeIsAParent2 = e "deedee is a parent of brassy"
    let bozoIsADog       = e "bozo is a dog"
    let deedeeIsAHorse   = e "deedee is a horse"

    let try1 = tryAssertion (e "(? animal) is a (? species)")
                            (e "bozo is a dog") []

    let try2 = tryAssertion (e "(? animal) is a parent of (? child)")
                            (e "deedee is a parent of sugar")
                           [(e "? species", e "dog"), (e "? animal",  e "bozo")]

    let try3 = tryAssertion (e "(? animal) is a (? species)")
                            (e "deedee is a horse") []

    let try4 = tryAssertion (e "(? animal) is a parent of (? child)")
                            (e "deedee is a parent of sugar")
                           [(e "? species", e "horse"), (e "? animal",  e "deedee")]


    let t36 = try1 == Stream
                        [(e "? species", e "dog"),
                         (e "? animal",  e "bozo")] EmptyStream
    let t37 = try2 == EmptyStream
    let t38 = try3 == Stream [(e "? species", e "horse"),
                              (e "? animal",  e "deedee")] EmptyStream

    let t39 = try4 == Stream [(e "? child",   e "sugar"),
                              (e "? species", e "horse"),
                              (e "? animal",  e "deedee")] EmptyStream

    let assertions = Stream (e "bozo is a dog")
                        (Stream (e "deedee is a horse")
                            (Stream (e "deedee is a parent of sugar")
                                (Stream (e "deedee is a parent of brassy") EmptyStream)))

    let identify = Rule "identify"
                    [e "(? animal) is a (? species)",
                     e "(? animal) is a parent of (? child)"]
                    (e "(? child)  is a (? species)")
    let rules = Stream identify EmptyStream
    let kb = Kb assertions rules

    let matchPTA1 = runState
                        (matchPatternToAssertions
                            (e "(? animal) is a (? species)") []) kb

    let matchPTA2 = runState
                        (matchPatternToAssertions
                            (e "(? animal) is a parent of (? child)")
                            [(e "? species", e "dog"),
                             (e "? animal",  e "bozo")]) kb

    let matchPTA3 = runState (
                        matchPatternToAssertions
                            (e "(? animal) is a parent of (? child)")
                            [(e "? species", e "horse"),
                             (e "? animal", e "deedee")]) kb

    let bs1 = Stream
                [(e "? species", e "dog"),
                 (e "? animal",  e "bozo")]
                (Stream [(e "? species", e "horse"),
                         (e "? animal",  e "deedee")] EmptyStream)

    let bs2 = Stream
                [(e "? child", e "sugar"),
                 (e "? species", e "horse"),
                 (e "? animal", e "deedee")]
                (Stream [(e "? child",   e "brassy"),
                         (e "? species", e "horse"),
                         (e "? animal",  e "deedee")] EmptyStream)

    let filterBs1 = runState
                        (filterBindingStream
                            (e "(? animal) is a (? species)") (Stream [] EmptyStream)) kb
    let filterBs2 = runState
                        (filterBindingStream
                            (e "(? animal) is a parent of (? child)") bs1) kb

    let filterBs3 = runState
                        (filterBindingStream
                            (e "(? animal) is a (? species)") (Stream [] EmptyStream) >>=
                        filterBindingStream
                            (e "(? animal) is a parent of (? child)")) kb

    let b1 = [(e "? species", e "horse"),
              (e "? child",   e "sugar"),
              (e "? animal",  e "deedee")]

    let b2 = [(e "? species", e "horse"),
              (e "? child",   e "brassy"),
              (e "? animal",  e "deedee")]

    let bs3 = Stream b1 (Stream b2 EmptyStream)

    let patterns = [e "(? animal) is a parent of (? child)",
                    e "(? animal) is a (? species)"]

    let applyFlt1 = runState (applyFilters patterns (Stream [] EmptyStream)) kb

    let inst1 = (e "sugar is a horse")
    let inst2 = (e "brassy is a horse")

    let instanVr1 = instantiateVariables (e "(? child) is a (? species)") b1
    let instanVr2 = instantiateVariables (e "(? child) is a (? species)") b2

    let kb2 = Kb EmptyStream EmptyStream
    let rmbAssrt1 = runState (rememberAssertion  (e "bozo is a dog") >>
                               rememberAssertion (e "deedee is a horse") >>
                               rememberAssertion (e "deedee is a parent of sugar") >>
                               rememberAssertion (e "deedee is a parent of brassy")) kb2

    let rmbAssrt2 = runState (rememberAssertion (e "bozo is a dog") >>
                              rememberAssertion (e "bozo is a dog")) kb2

    let rmbRules1 = runState (rememberRule identify) kb2
    let rmbRules2 = runState (rememberRule identify >> rememberRule identify) kb2

    let assertions2 = Stream (e "bozo is a dog")
                        (Stream (e "deedee is a horse")
                            (Stream (e "deedee is a parent of sugar")
                                (Stream (e "deedee is a parent of brassy")
                                    (Stream (e "sugar is a horse")
                                        (Stream (e "brassy is a horse") EmptyStream)))))

    let useRule1 = runStateT (useRule identify) kb

    let t40 = matchPTA1 == (Stream
                                [(e "? species", e "dog"),
                                 (e "? animal",  e "bozo")]
                                (Stream [(e "? species", e "horse"),
                                         (e "? animal",  e "deedee")] EmptyStream), kb)

    let t41 = matchPTA2 == (EmptyStream, kb)
    let t42 = matchPTA3 == (Stream
                                [(e "? child",   e "sugar"),
                                 (e "? species", e "horse"),
                                 (e "? animal",  e "deedee")]
                                (Stream [(e "? child",   e "brassy"),
                                         (e "? species", e "horse"),
                                         (e "? animal",  e "deedee")] EmptyStream), kb)

    let t43 = filterBs1 == (bs1, kb)
    let t44 = filterBs2 == (bs2, kb)
    let t45 = filterBs3 == (bs2, kb)

    let t46 = applyFlt1 == (bs3, kb)

    let t47 = instanVr1 == inst1
    let t48 = instanVr2 == inst2
    let t49 = rmbAssrt1 == (True, Kb assertions EmptyStream)
    let t50 = rmbAssrt2 == (False, Kb (Stream (e "bozo is a dog") EmptyStream) EmptyStream)
    let t51 = rmbRules1 == (True, Kb EmptyStream rules)
    let t52 = rmbRules2 == (False, Kb EmptyStream rules)

    let t53 = do
        (bool, state) <- useRule1
        if (bool, state) == (True, Kb assertions2 rules)
            then putStrLn " OK"
            else do putStrLn " Fail"
                    exitFailure

    mapM_ testa [t36, t37, t38, t39, t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50, t51, t52]

    t53

    runStateT forwardChain kb

    return ()

testa :: Bool -> IO ()
testa t =
    if t then putStrLn " OK"
         else do putStrLn " Fail"
                 exitFailure
