module Complexity (fac,showComplexity) where

import Data.List

data BigOFamily = O1 | OLogn | On | OnLogn | Onp2 | O2pn | Ofacn deriving (Enum, Bounded, Show)

fac :: (Num a, Eq a, Enum a) => a -> a
fac 0 = 1
fac n = foldl' (*) 1 [1..n]

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
