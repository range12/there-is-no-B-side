module Complexity (showComplexity) where


data BigOFamily = O1 | OLogn | On | OnLogn | Onp2 | O2pn | Ofacn deriving (Enum, Show)

fac :: (Num a) => a -> a
fac 0 = 1
fac n = foldl' (*) [1..n]

showComplexity :: (Integral a, Integral b) => (a,b) -> String
showComplexity p =
    let inputSize = fromIntegral (fst p) :: Float -- doc as N
    let nOperations = fromIntegral (snd p) :: Float -- doc as Q
    let transforms =
        -- O1: N * 1 ~= Q
        (subtract nOperations)
        -- OLog(n) : N*Log(N) ~= nOp
        :(subtract nOperations . (\n -> n * log n))
        -- On : N * N^1 ~= nOP
        :(subtract nOperations . (** 2))
        -- OnLog(n) : N * N^1 * Log(N) ~= nOP
        :(subtract nOperations . (\n -> (n ** 2) * log n))
        -- On^2 : N * N^2 ~= nOP
        :(subtract nOperations . (** 3))
        -- O2^n : N * 2^N ~= nOP
        :(subtract nOperations . (\n -> n * 2 ** n)
        -- On! : N * N! ~= nOP
        :(subtract nOperations . (\n -> n * fac n))
        :[]
    in let complexities = zip 
        [O1..Ofacn]
        (map inputSize transforms)
    let cleaned = sortBy (\l r -> snd l - snd r)$ filter (\p -> snd p > 0) complexities
    let dbg = "Computed delta zips:\n" ++ (unlines$ map show complexities)
    dbg ++ "Complexity: " ++ (show$ head$ cleaned)
