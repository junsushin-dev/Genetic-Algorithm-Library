-- CPSC 312 - 2018 - Genetic Algorithm Library
module BitArrayDNA (
    Chromosome,
    Dna,
    mkDna,
    mkData,
    getFst,
    getSnd
    ) where

-- defines single chromosome data, being either 0 or 1
data Chromosome = ChromosomeF Int
    deriving (Show, Eq)

-- insures that the Chromosome only has values 0 or 1
mkChromosome :: Int -> Chromosome
mkChromosome n
    | n == 1 || n == 0 = ChromosomeF n
    | otherwise = error "Invalid Integer"

-- defines the whole Dna
-- Dna is a list of 6 chromosomes
data Dna = DnaF [Chromosome]
    deriving (Show, Eq)

-- ensures that Dna has a list lenght of 6
mkDna :: [Chromosome] -> Dna
mkDna list
    | length list == 6 = DnaF list
    | otherwise = error "Invalid length"

-- easy constructor function for Dna
mkData :: [Int] -> Dna
mkData lst = mkDna (map mkChromosome lst)

dna2lst :: Dna -> [Chromosome]
dna2lst (DnaF lst) = lst

-- test cases 
-- mkData [1,1,1,1,1,1]
-- mkData [1,1,1,1,0,1]
-- mkData [1,1,1,1,0,0,0]
-- mkData [1,1,1,1,0,2]

-- gets the first half of the Dna
getFst :: Dna -> [Chromosome]
getFst dna = fst (splitAt 3 (dna2lst dna))

-- gets the second half of the Dna
getSnd :: Dna -> [Chromosome]
getSnd dna = snd (splitAt 3 (dna2lst dna))

-- test cases
-- foo = mkData[1,1,1,0,0,0]
-- getFst foo
-- getSnd foo