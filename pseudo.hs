-- CPSC 312 - 2018 - Genetic Algorithm Library

import Cross
import Mutate

import Control.Monad (replicateM)
import Data.List
import Data.Maybe
import System.Random

type Population = [Chromosome]
type Fit = (Chromosome -> Double)

----- Client API types Definition

-- TBD
{-
-- Fitness function. From a Chromosome it must return a Num regarding how good this Chromosome is
fit::Chromosome -> Num

-- mutate a single Chromosome according to pm(mutation probability)
mutate::Chromosome -> Double -> Chromosome

-- data result = (Chromosome, population) -- best Chromosome, whole population
-}

genetic fit cross mutate pc pm maxIterations population =
    do
        rg <- newStdGen     -- gets a new random number generater each time
        let best = appliedGenetic fit cross mutate pc pm maxIterations population (randomRs (0, ((length population)-1) :: Int) rg) (randomRs (0, 1 :: Double) rg)
        return best

appliedGenetic fit _ _ _ _ 0 population _ _= population !! (indexOfBest fit population)
appliedGenetic fit cross mutate pc pm maxIterations population intRndStream realRndStream =
    appliedGenetic fit cross mutate pc pm (maxIterations-1) replacePop intTail realTail
        where
            best = population !! (indexOfBest fit population)
            (rndIndex, intTail) = splitAt (2 * (length population)) intRndStream
            selectPop = select fit population rndIndex 0
            (rndCrossProbs, tempTail) = splitAt (div (length population) 2) realRndStream
            crossPop = crossAll cross pc rndCrossProbs selectPop
            (rndMutProb, realTail) = splitAt (length population) tempTail
            mutatePop = [if p < pm then mutate c else c | (c, p) <- zip population rndMutProb]
            replacePop = replacement fit mutatePop best

--genetic fit _ _ _ _ 0 population = do return population !! (indexOfBest fit population)
--genetic fit cross mutate pc pm maxIterations population =
--    do
--        rndIndex <- replicateM (2 * length population) $ randomRIO (0, (length population - 1) :: Int)
--        rndCrossProbs <- replicateM (length population / 2) $ randomRIO (0, 1 :: Double)
--        rndMutProb <- replicateM (length population) $ randomRIO (0, 1 :: Double)
--
--        let best = population !! (indexOfBest fit population)
--        let selectPop = select fit population rndIndex 0
--        let crossPop = crossAll cross pc rndCrossProbs selectPop
--        let mutatePop = [if p < pm then mutate c else c | (c, p) <- zip population rndMutProb]
--        let replacePop = replacement fit mutatePop best
--
--        return genetic fit cross mutate pc pm (maxIterations-1) replacePop

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
binaryTournament:: Fit -> Population -> Int -> Int -> Chromosome  
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
select:: Fit -> Population -> [Int] -> Int -> Population
select fit population (i1:(i2:t)) i
    | i >= length population = population
    | otherwise = select fit (replaceNth population new i) t (i+1)
    where
        new = binaryTournament fit population i1 i2

-- make sure that the best genes are selected into the population (elitism)
replacement:: Fit -> Population -> Chromosome -> Population
replacement fit population best = replaceNth population best worst
    where
        worst = indexOfWorst fit population

-- get the index of the worst solution in a population
indexOfWorst fit lst = fromJust (elemIndex (minimum fittedLst) fittedLst) 
    where
        fittedLst = map fit lst

--get the index of the best solution in a population
indexOfBest fit lst = fromJust (elemIndex (maximum fittedLst) fittedLst) 
    where
        fittedLst = map fit lst