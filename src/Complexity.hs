module Complexity (fac,showComplexity) where

import Data.List

data BigOFamily = O1 | OLogn | On | OnLogn | Onp2 | O2pn | Ofacn deriving (Enum, Bounded)

instance Show BigOFamily where
    show O1 = "O(1)"
    show OLogn = "O(log n)"
    show On = "O(n)"
    show OnLogn = "O(n*log n)"
    show Onp2 = "O(n²)"
    show O2pn = "O(2ⁿ)"
    show Ofacn = "O(n!)"

fac :: (Num a, Eq a, Enum a) => a -> a
fac 0 = 1
fac n = foldl' (*) 1 [1..n]

-- Takes a couple of integers :
-- be N: the size of the input, in symbols
-- be Q: the number of operations upon successfully terminating a computation.
-- Computes a prospective value Q' for each time complexity profile;
-- Computes the Q/Q' ratio;
-- Elects the complexity profile whose ratio is the closest to 1.0.

showComplexity :: (Integral a, Integral b) => (a,b) -> String
showComplexity p =
    let inputSize = fromIntegral (fst p) :: Double -- doc as N
        nOperations = fromIntegral (snd p) :: Double -- doc as Q
        transforms =
            -- O1: N * 1 ~= Q
            ((/) nOperations)
            -- OLog(n) : N*Log(N) ~= Q
            :((/) nOperations . (\n -> n * log n))
            -- On : N * N^1 ~= Q
            :((/) nOperations . (** 2))
            -- OnLog(n) : N * N^1 * Log(N) ~= Q
            :((/) nOperations . (\n -> (n ** 2) * log n))
            -- On^2 : N * N^2 ~= Q
            :((/) nOperations . (** 3))
            -- O2^n : N * 2^N ~= Q
            :((/) nOperations . (\n -> n * 2 ** n))
            -- On! : N * N! ~= Q
            :((/) nOperations . (\n -> n * fac n))
            :[]
            -- Ordering: Decreasing absolute proximity to 1.0
        in let closestToOne (_,l) (_,r) = compare (abs$ 1 - l) (abs$ 1 - r)
               complexities = sortBy closestToOne
                    $zip
                    ([minBound .. maxBound] :: [BigOFamily])
                    (map (\f -> f inputSize) transforms)
               dbg = "Computed delta zips:\n" ++ (unlines$ map show complexities)
              in do
    dbg ++ "Complexity: " ++ (show$ head$ complexities)
