
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



    let pOfn_1 = (20, round (20 *  fac 16))
    let pOfn_2 = (20, round (20 * fac 24))
    let pOfn_3 = (20, round (20 * fac 20))
    putStrLn $ "\nO(n!) test with Q ~= (N*4/5)! * N : " ++ show pOfn_1
    putStrLn $ showComplexity pOfn_1
    putStrLn $ "\nO(n!) test with Q ~= (N*6/5)! * N : " ++ show pOfn_2
    putStrLn $ showComplexity pOfn_2
    putStrLn $ "\nO(n!) test with Q ~= N! * N : " ++ show pOfn_3
    putStrLn $ showComplexity pOfn_3
