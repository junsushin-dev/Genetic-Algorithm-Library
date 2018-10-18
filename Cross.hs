-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin

module Cross (
    cross,
    Chromosome
    ) where

import BitArrayChromosome

-- cross two chromosomes to make a pair of children
{-
dad = mkData [0,0,0,0,0,1]
mom = mkData [0,1,0,0,1,1]
cross dad mom
-}

cross::Chromosome -> Chromosome -> (Chromosome, Chromosome)
cross a b = crossTuple (a, b)

crossTuple::(Chromosome, Chromosome) -> (Chromosome, Chromosome)
crossTuple (parent1, parent2) = (child1, child2) where
    child1 = mkChromosome ((getFst parent1)++(getSnd parent2))
    child2 = mkChromosome ((getFst parent2)++(getSnd parent1))


{- Old Junsu's Version of CrossAll
crossAll _ _ [] = []
crossAll crossfunc cp population =  flattenTupleList (map crossfunc (toTuplelst population))
-}



-- cuts the list of Chromosome into a list of 2 pieces
-- toTuplelst [1,2,3,4,5,6]
-- [[1,2],[3,4],[5,6]]

{-
toTuplelst::[Chromosome] -> [(Chromosome, Chromosome)]
toTuplelst [] = []
toTuplelst (h:t) 
    | t == [] = error "Uneven number of elements"
    | otherwise = (h,head t):(toTuplelst t)

-- make the list of tuple into a big concatonated list 
flattenTupleList [] = []
flattenTupleList ((fstelt, sndelt):t) = [fstelt, sndelt]++(flattenTupleList t)
-}