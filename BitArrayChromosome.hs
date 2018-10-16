-- CPSC 312 - 2018 - Genetic Algorithm Library
module BitArrayChromosome (
    Chromosome,
    Bin,
    mkChromosome,
    mkData,
    getFst,
    getSnd
    ) where

-- defines single chromosome data, being either 0 or 1
data Bin = Bin Int
    deriving (Show, Eq)

-- insures that the Chromosome only has values 0 or 1
mkBin :: Int -> Bin
mkBin n
    | n == 1 || n == 0 = Bin n
    | otherwise = error "Invalid Integer"

-- defines the whole chromosome
-- chromosome is a list of 6 chromosomes
data Chromosome = Chromosome [Bin]
    deriving (Show, Eq)

-- ensures that chromosome has a list lenght of 6
mkChromosome :: [Bin] -> Chromosome
mkChromosome list
    | length list == 6 = Chromosome list
    | otherwise = error "Invalid length"

-- easy constructor function for chromosome
mkData :: [Int] -> Chromosome
mkData lst = mkChromosome (map mkBin lst)

chromosome2lst :: Chromosome -> [Bin]
chromosome2lst (Chromosome lst) = lst

-- test cases 
-- mkData [1,1,1,1,1,1]
-- mkData [1,1,1,1,0,1]
-- mkData [1,1,1,1,0,0,0]
-- mkData [1,1,1,1,0,2]

-- gets the first half of the chromosome
getFst :: Chromosome -> [Bin]
getFst chromosome = fst (splitAt 3 (chromosome2lst chromosome))

-- gets the second half of the chromosome
getSnd :: Chromosome -> [Bin]
getSnd chromosome = snd (splitAt 3 (chromosome2lst chromosome))

-- test cases
-- foo = mkData[1,1,1,0,0,0]
-- getFst foo
-- getSnd foo