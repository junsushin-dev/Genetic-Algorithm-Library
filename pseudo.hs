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

genetic _ _ _ _ _ 0 population = population 
genetic fit cross mutate pc pm maxIterations population = genetic fit cross mutate pc pm maxIterations-1 replacePop (mutatePop pm (crossPop pc (selectPop population)))
    where
        selectPop population = select fit population
        crossPop population = cross population
        mutatePop population = [mutate c | c <- population | shouldApplyMut pm] -- generate random number in shouldApplyMut
        replacePop population = replacement fit population -- TODO: missing parameter "best"

-- population::[[]]
-- apply it n times (n = number of population) maybe fold or map (a is dummy for map use)
binaryTournament::fit -> population -> chromosome  
binaryTournament fit population =
    do
        let s1 = population!!randomRIO(0, length population)
        let s2 = population!!randomRIO(0, length population)

        if fit s1 > fit s2 then s1 else s2


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
        worst = indexofworst fit population

-- get the index of the worst solution in a population
indexofworst fit lst = elemIndex (minimum fittedLst) fittedLst 
    where
        fittedLst = map fit lst


-- Return True if generated number is below pm.
shouldApplyMut pm = if pm < (randomRIO (0, 1 :: Double)) then True else False

-- cross the population to generate new population
crossALL::cross -> pc -> population -> population

mutateALL::mutate -> population -> population

