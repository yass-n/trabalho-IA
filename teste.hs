import Basics
import Match

main:: IO ()
main = do
    putStrLn "teste do match"

    let x = Variavel 'X'
    let y = Variavel 'Y'
    let underscore = Variavel '_'

    let apple = Constante "apple"
    let red = Constante "red"
    let orange = Constante "orange"

    let p1 = Predicado "color" [x,red]
    let p2 = Predicado "color" [apple, red]
    let p3 = Predicado "color" [apple, y]
    let p4 = Predicado "color" [x,y]
    let p5 = Predicado "color" [underscore, red]
    let p6 = Predicado "color" [underscore, underscore]
    let p7 = Predicado "color" [apple, orange]
    let p8 = Predicado "color" [x,x]

    let t1 = match p1 p2 == Just [Ligacao 'X' "apple"]
    let t2 = match p3 p2 == Just [Ligacao 'Y' "red"]
    let t3 = match p4 p2 == Just [Ligacao 'Y' "red", Ligacao 'X' "apple"]
    let t4 = match p2 p2 == Just []
    let t5 = match p5 p2 == Just []
    let t6 = match p6 p2 == Just []
    let t7 = match p7 p2 == Nothing
    let t8 = match p8 p2 == Nothing


    mapM_ testa [t1, t2, t3, t4, t5, t6, t7, t8]

testa :: Bool -> IO ()
testa t = do
    if t then putStrLn " OK"
         else putStrLn " Fail"
