-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Salva, Ofir, Junsu
-- Module: TestMain module for the user to import and use

module TestMain(
    genBinPop,
    mkData,
    genetic,
    printResult
    )

where

import Cross
import Mutate
import BitArrayFit
import BitArrayChromosome
import Population

import Control.Monad (replicateM)
import Data.List
import Data.Maybe
import System.Random

type Population = [Chromosome]

----- Client API types Definition

genetic fit cross mutate pc pm maxIterations ioPopulation chromosomeSize =
    do
        rg <- newStdGen     -- gets a new random number generater each time
        population <- ioPopulation
        let intRndStream = (randomRs (0, ((length population)-1) :: Int) rg)
        let realRndStream = (randomRs (0, 1 :: Double) rg)
        let muteStream = (randomRs (0, (chromosomeSize - 1) :: Int) rg)
        let best = appliedGenetic fit cross mutate pc pm maxIterations population intRndStream realRndStream muteStream
        return best

appliedGenetic fit _ _ _ _ 0 population _ _ _= ((population !! (indexOfBest fit population)) , [])
appliedGenetic fit cross mutate pc pm maxIterations population intRndStream realRndStream muteStream =
    (sol, best : bestList)
        where
            best = population !! (indexOfBest fit population)
            (rndIndex, intTail) = splitAt (2 * (length population)) intRndStream
            selectPop = select fit population rndIndex 0
            (rndCrossProbs, tempTail) = splitAt (div (length population) 2) realRndStream
            crossPop = crossAll cross pc rndCrossProbs selectPop
            (rndMutProb, realTail) = splitAt (length population) tempTail
            (muteList, muteTail) = splitAt (length population) muteStream
            muteRandNums = zip muteList rndMutProb
            mutatePop = [if p < pm then mutate c i else c | (c, (i, p)) <- zip population muteRandNums]
            replacePop = replacement fit mutatePop best
            (sol, bestList) = appliedGenetic fit cross mutate pc pm (maxIterations-1) replacePop intTail realTail muteTail

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

-- <Test Case> for BitArray type Chromosome of length 10
{- 
let testPop = genBinPop 10 10
let target = dec2bin 10 1023
let myfit = targetCompare target
let res = genetic myfit cross mutate 0.3 0.05 50 p 10
printResult res
-}

-- prints the result of genetic to be seen by the user
-- prints,
-- 1. the best chromosome from the last generation according to the fitness function
-- 2. the list of best candidates for each generation
printResult :: IO (Chromosome, [Chromosome]) -> IO ()
printResult res =
    do
        (final, record) <- res
        let finalString = "Final Best: " ++ (show final) ++ "\n"
        let recordString = recordPrint 1 record
        wholeString <- putStr (finalString ++ recordString)
        return wholeString

-- adds the generation number and provides a line switch for better visibility
recordPrint :: Int -> [Chromosome] -> String
recordPrint _ [] = ""
recordPrint i (h:t) = "Gen " ++ (show i) ++  ": " ++ (show h) ++ "\n" ++ (recordPrint (i+1) t)