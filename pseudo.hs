-- CPSC 312 - 2018 - Genetic Algorithm Library

import Data.List
import Cross

type Population = [Chromosome]

----- Client API types Definition

-- TBD
{-
-- Fitness function. From a Chromosome it must return a Num regarding how good this Chromosome is
fit::Chromosome -> Num

-- mutate a single Chromosome according to pm(mutation probability)
mutate::Chromosome -> Double -> Chromosome

-- data result = (Chromosome, population) -- best Chromosome, whole population
-}

genetic _ _ _ _ _ 0 population = population !! indexOfBest population
genetic fit cross mutate pc pm maxIterations population = genetic fit cross mutate pc pm maxIterations-1 replacePop best (mutatePop pm (crossPop pc (selectPop population)))
    where
        best = population !! indexOfBest population
        selectPop pop = select fit pop
        crossPop pop = crossAll pc cross pop
        mutatePop pop = [mutate c | c <- pop, shouldApply pm] -- TODO: pass random p as an argument. Maybe with c <- pop p <- probs, shouldApplyMut pm p
        replacePop b pop = replacement fit pop b

-- typedef for crossAll
type Crossfunc = Chromosome -> Chromosome -> (Chromosome, Chromosome)
type Crossprob = Double

-- cross the population to generate new population
-- We will cross contiguous parents (i, i+1). If a random double in [0,1] is less than pc,
-- replace i and i+1 by the pair of new chromosomes returned from cross function
crossAll::Crossfunc -> Crossprob -> [Double] -> Population -> Population
crossAll _ _ _ [] = []
crossAll cross pc (p:t) (a:(b:pop)) = if (p < pc) then sa : (sb : crossedPop) else a : (b : crossedPop)
    where
        (sa, sb) = cross a b
        crossedPop = crossAll cross pc t pop

-- apply it n times (n = number of population) maybe fold or map (a is dummy for map use)
binaryTournament::Integral a => fit -> Population -> a -> a -> Chromosome  
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
select::fit -> Population -> Population
select fit population = map (replaceNth population new) [0..(length population)]
    where
        new = binaryTournament fit population

-- make sure that the best genes are selected into the population (elitism)
replacement::fit -> Population -> Population
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
shouldApply pm p = p < pm