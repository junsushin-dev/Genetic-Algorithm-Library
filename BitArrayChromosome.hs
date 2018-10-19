-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin
-- Module: BitArrayChromosome
-- This module specifies the data type for a BitArray typed Chromosome of length 6
-- Built for test case / prototyping purposes

module BitArrayChromosome (
    Chromosome,
    Bin,
    Rand,
    --chromosome2lst,
    mkChromosome,
    mkData,
    getFst,
    getSnd,
    mutategene,
    genChromosome,
    dec2bin
    ) where

import System.Random

-- defines single chromosome data, being either 0 or 1
data Bin = Bin Int
    deriving (Show, Eq)

-- insures that the Chromosome only has values 0 or 1
mkBin :: Int -> Bin
mkBin n
    | n == 1 || n == 0 = Bin n
    | otherwise = error "Invalid Integer"

-- defines a chromosome
-- chromosome is a list of 6 genes
type Chromosome = [Bin]

-- ensures that chromosome has a lenght of 6
mkChromosome :: [Bin] -> Chromosome
mkChromosome list
    | length list == 6 = list
    | otherwise = error "Invalid length"

-- easy constructor function for chromosome
mkData :: [Int] -> Chromosome
mkData lst = map mkBin lst

{-
chromosome2lst :: Chromosome -> [Bin]
chromosome2lst (Chromosome lst) = lst
-}

-- test cases 
-- mkData [1,1,1,1,1,1]
-- mkData [1,1,1,1,0,1]
-- mkData [1,1,1,1,0,0,0]
-- mkData [1,1,1,1,0,2]

-- gets the first half of the chromosome
getFst :: Chromosome -> [Bin]
getFst chromosome = fst (splitAt 3 chromosome)

-- gets the second half of the chromosome
getSnd :: Chromosome -> [Bin]
getSnd chromosome = snd (splitAt 3 chromosome)

-- test cases
-- foo = mkData[1,1,1,0,0,0]
-- getFst foo
-- getSnd foo

-- Flips the bit of the selected gene in a Chromosome 
mutategene :: Chromosome -> Int -> Chromosome
mutategene ch i = replaceNth ch newv i 
    where
        newv = if ch!!i == Bin 0 then Bin 1 else Bin 0

-- Replace the nth element in a list by newVal
replaceNth :: [a] -> a -> Int -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) newVal n
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth xs newVal (n-1)

type Rand = Int -- of scope between 0~63

-- Generates a Binary gene Chromosome of length 6
genChromosome :: Rand -> Chromosome
genChromosome i = dec2bin 6 i

-- Transforms Rand data into Binary Array form
dec2bin :: Int -> Int -> [Bin]
dec2bin len todiv
    | len == 0 = []
    | otherwise = (dec2bin (len-1) (div todiv 2))++[Bin (mod todiv 2)]