-- CPSC 312 - 2018 - Genetic Algorithm Library

import Data.List

genetic _ _ _ _ _ 0 population = population 
genetic fit cross mutate pc pm maxIterations population =  genetic fit cross mutate pc pm maxIterations-1 replacePop (mutatePop pm (crossPop pc (selectPop population)))
    where
        selectPop population = select fit population
        crossPop population = cross population
        mutatePop population = [mutate c | c <- population | shouldApplyMut pm] -- generate random number in shouldApplyMut
        replacePop population = replacement population

data result = (chromosome, population) -- best chromosome, whole population

data chromosome = [a]

fit::chromosome -> Num

-- cross two chromosomes to make a pair of children
cross::chromosome -> chromosome -> (chromosome, chromosome)

-- mutate a single chromosome
mutate::chromosome -> pm -> chromosome

population::[chromosome]

-- population::[[]]
-- apply it n times (n = number of population) maybe fold or map
binaryTournament::fit -> population -> chromosome  
binaryTournament fit population =
    do
        n1 <- randomIO :: IO Interger
        n2 <- randomIO :: <- randomIO :: IO Interger

        let s1 = population!!n1
        let s2 = population!!n2

        if fit s1 > fit s2 then s1 else s2

select::fit -> population -> population

-- make sure that the best genes are selected into the population (elitism)
replacement::fit -> population -> population

replacement fit population best = 
    i = indexofworst fit population
    population!!i = best 

-- get the index of the worst solution in a population

indexofworst fit lst = elemIndex (minimum fittedLst) fittedLst where
    fittedLst = map fit lst
--indexofworst fit [] (worstFit, worstIndex) = (worstFit, worstIndex)
--indexofworst fit [h:t] (worstFit, worstIndex) = if fit h < worstFit then indexofworst fit t ()

-- cross the population to generate new population
crossALL::cross -> pc -> population -> population

mutateALL::mutate -> population -> population

