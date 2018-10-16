-- CPSC 312 - 2018 - Genetic Algorithm Library

import Data.List

----- Client API types Definition
-- Fitness function. From a chromosome it must return a Num regarding how good this chromosome is
fit::chromosome -> Num

-- cross two chromosomes to make a pair of children
cross::chromosome -> chromosome -> (chromosome, chromosome)

-- mutate a single chromosome
mutate::chromosome -> pm -> chromosome

population::[chromosome]
chromosome::[a]

-- data result = (chromosome, population) -- best chromosome, whole population
-- data chromosome = [a]

genetic _ _ _ _ _ 0 population = population !! indexOfBest population
genetic fit cross mutate pc pm maxIterations population = genetic fit cross mutate pc pm maxIterations-1 replacePop best (mutatePop pm (crossPop pc (selectPop population)))
    where
        best = indexOfBest population
        selectPop pop = select fit pop
        crossPop pop = crossAll pc cross pop
        mutatePop pop = [mutate c | c <- pop | shouldApplyMut pm] -- generate random number in shouldApplyMut
        replacePop b pop = replacement fit pop b

-- population::[[]]
-- apply it n times (n = number of population) maybe fold or map (a is dummy for map use)
binaryTournament::Integral a => fit -> population -> a -> a -> chromosome  
binaryTournament fit population i1 i2 = if (fit s1) > (fit s2) then s1 else s2
    where
        s1 = population!!i1
        s2 = population!!i2

-- Replace the nth element in a list by newVal
replaceNth :: [a] -> a -> Int -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) newVal n
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth xs newVal (n-1)

-- Select a Population using binary tournament
select::fit -> population -> population
select fit population = map (replaceNth population new) [0..(length population)]
    where
        new = binaryTournament fit population

-- make sure that the best genes are selected into the population (elitism)
replacement::fit -> population -> population
replacement fit population best = replaceNth population best worst
    where
        worst = indexOfWorst fit population

-- get the index of the worst solution in a population
indexOfWorst fit lst = elemIndex (minimum fittedLst) fittedLst 
    where
        fittedLst = map fit lst

--get the index of the best solution in a population
indexOfBest fit lst = elemIndex (maximum fittedLst) fittedLst 
    where
        fittedLst = map fit lst

-- Return True if generated number is below pm.
shouldApplyMut pm p = if p < pm then True else False

-- cross the population to generate new population
-- We will cross contiguous parents (i, i+1). If a random double in [0,1] is less than pc,
-- replace i and i+1 by the pair of new chromosomes returned from cross function
crossALL:: (Integral a) => cross -> pc -> population -> population

crossAll _ [] = []
crossAll pc (a:b:pop) p = if (p < pc) then sa : sb : crossedPop else a : b : crossedPop
    where
        (sa, sb) = cross a b
        crossedPop = crossAll pc pop