import Tipos
import Match

main:: IO ()
main = do
    putStrLn "teste do match"

    let x = Variavel "x"
    let y = Variavel "y"
    let underscore = Variavel "_"

    let apple = Atomo "apple"
    let red = Atomo "red"
    let orange = Atomo "orange"
    let color = Atomo "color"

    let p1 = color `Seq` x `Seq` red
    let p2 = color `Seq` apple `Seq` red
    let p3 = color `Seq` apple `Seq`  y
    let p4 = color `Seq` x `Seq` y
    let p5 = color `Seq` underscore `Seq`  red
    let p6 = color `Seq` underscore `Seq`  underscore
    let p7 = color `Seq` apple `Seq`  orange
    let p8 = color `Seq` x `Seq` x

    let t1 = match p1 p2 [] == Just [(x, apple)]
    let t2 = match p3 p2 [] == Just [(y, red)]
    let t3 = match p4 p2 [] == Just [(y, red), (x, apple)]
    let t4 = match p2 p2 [] == Just []
    let t5 = match p5 p2 [] == Just []
    let t6 = match p6 p2 [] == Just []
    let t7 = match p7 p2 [] == Nothing
    let t8 = match p8 p2 [] == Nothing


    mapM_ testa [t1, t2, t3, t4, t5, t6, t7, t8]

testa :: Bool -> IO ()
testa t = do
    if t then putStrLn " OK"
         else putStrLn " Fail"
