
import Complexity

main = do
    -- (inputSize, nOperations):
    -- case nOp ~= 2.5 inputSize => O(1)
    let pO1_1 = (10000, round (10000 * 2.5))
    -- case nOp ~= 0.9 inputSize => O(1)
    let pO1_2 = (20000, round (20000 * 0.99))
    let pO1_3 = (20000, round (20000 * 1.0))
    putStrLn "Testing complexity inference, comparing N: inputSize to Q: operation amount."
    putStrLn $ "\nO(1) test with Q ~= 2.5N : " ++ show pO1_1
    putStrLn $ showComplexity pO1_1
    putStrLn $ "\nO(1) test with Q ~= 0.99N : " ++ show pO1_2
    putStrLn $ showComplexity pO1_2
    putStrLn $ "\nO(1) test with Q ~= 1N : " ++ show pO1_3
    putStrLn $ showComplexity pO1_3



    let pOln_1 = (10000, round (10000 *  log 15000))
    let pOln_2 = (10000, round (10000 * log 8000 ))
    let pOln_3 = (10000, round (10000 * log 10000))
    putStrLn $ "\nO(ln) test with Q ~= Log(1.5N) * N : " ++ show pOln_1
    putStrLn $ showComplexity pOln_1
    putStrLn $ "\nO(ln) test with Q ~= Log(0.8N) * N : " ++ show pOln_2
    putStrLn $ showComplexity pOln_2
    putStrLn $ "\nO(ln) test with Q ~= Log(N) * N : " ++ show pOln_3
    putStrLn $ showComplexity pOln_3


    let pOn_1 = (10000, round (10000 *  15500))
    let pOn_2 = (10000, round (10000 * 8000))
    let pOn_3 = (10000, round (10000 ** 2))
    putStrLn $ "\nO(n) test with Q ~= 1.5N * N : " ++ show pOn_1
    putStrLn $ showComplexity pOn_1
    putStrLn $ "\nO(n) test with Q ~= 0.8N * N : " ++ show pOn_2
    putStrLn $ showComplexity pOn_2
    putStrLn $ "\nO(n) test with Q ~= N * N : " ++ show pOn_3
    putStrLn $ showComplexity pOn_3


    let i1 = 1350
    let pOnLogn_1 = (1000, round (1000 *  i1 * log i1))
    let i2 = 800
    let pOnLogn_2 = (1000, round (1000 * i2 * log i2))
    let pOnLogn_3 = (1000, round (1000 ** 2 * log 1000))
    putStrLn $ "\nO(nLog(n)) test with Q ~= M=(1.3N) MLog(M) * N : " ++ show pOnLogn_1
    putStrLn $ showComplexity pOnLogn_1
    putStrLn $ "\nO(nLog(n)) test with Q ~= M=(0.8N) MLog(M) * N : " ++ show pOnLogn_2
    putStrLn $ showComplexity pOnLogn_2
    putStrLn $ "\nO(nLog(n)) test with Q ~= NLog(N) * N : " ++ show pOnLogn_3
    putStrLn $ showComplexity pOnLogn_3


    let pOnn_1 = (1000, round (1000 *  1350 ** 2))
    let pOnn_2 = (1000, round (1000 * 800 ** 2))
    let pOnn_3 = (1000, round (1000 ** 3))
    putStrLn $ "\nO(n^2) test with Q ~= (1.3N)^2 * N : " ++ show pOnn_1
    putStrLn $ showComplexity pOnn_1
    putStrLn $ "\nO(n^2) test with Q ~= (0.8N)^2 * N : " ++ show pOnn_2
    putStrLn $ showComplexity pOnn_2
    putStrLn $ "\nO(n^2) test with Q ~= N^2 * N : " ++ show pOnn_3
    putStrLn $ showComplexity pOnn_3



    let pO2Pn_1 = (50, round (50 *  2 ** 67))
    let pO2Pn_2 = (50, round (50 * 2 ** 40))
    let pO2Pn_3 = (50, round (50 * 2 ** 50))
    putStrLn $ "\nO(2^n) test with Q ~= 2^(1.3N) * N : " ++ show pO2Pn_1
    putStrLn $ showComplexity pO2Pn_1
    putStrLn $ "\nO(2^n) test with Q ~= 2^(0.8N) * N : " ++ show pO2Pn_2
    putStrLn $ showComplexity pO2Pn_2
    putStrLn $ "\nO(2^n) test with Q ~= 2^N * N : " ++ show pO2Pn_3
    putStrLn $ showComplexity pO2Pn_3


    let pOfn_1 = (20, round (20 *  fac 16))
    let pOfn_2 = (20, round (20 * fac 24))
    let pOfn_3 = (20, round (20 * fac 20))
    putStrLn $ "\nO(n!) test with Q ~= (N*4/5)! * N : " ++ show pOfn_1
    putStrLn $ showComplexity pOfn_1
    putStrLn $ "\nO(n!) test with Q ~= (N*6/5)! * N : " ++ show pOfn_2
    putStrLn $ showComplexity pOfn_2
    putStrLn $ "\nO(n!) test with Q ~= N! * N : " ++ show pOfn_3
    putStrLn $ showComplexity pOfn_3
