-- CPSC 312 - 2018 - Genetic Algorithm Library
-- done by Junsu Shin

module BitArrayFit

where

import BitArrayChromosome

type Fit = Chromosome -> Int

temptarget = mkData [1,1,1,1,1,1]

-- Fitness function. From a Chromosome it must return a Num regarding how good this Chromosome is
-- <Test Case>
{-
d1 = mkData [0,0,1,0,0,0]
d3 = mkData [0,1,0,1,0,1]
d4 = mkData [1,0,1,0,1,1]
d6 = mkData [1,1,1,1,1,1]
fit d1
fit d3
fit d4
fit d6
-}
fit::Chromosome -> Int
fit chromosome = targetCompare temptarget chromosome

targetCompare:: (Eq a) => [a] -> [a] -> Int
targetCompare _ [] = 0
targetCompare [] _ = 0
targetCompare (h1:t1) (h2:t2)
    | h1 == h2 = 1 + targetCompare t1 t2
    | otherwise = targetCompare t1 t2