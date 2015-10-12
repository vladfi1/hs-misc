module Sort where

merge :: (Ord a) => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) =
  if a <= b
    then a : merge as (b:bs)
    else b : merge (a:as) bs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort as = merge (mergeSort first) (mergeSort second)
  where (first, second) = splitAt (length as `quot` 2) as

pivot :: (Ord a) => a -> [a] -> ([a], [a])
pivot _ [] = ([], [])
pivot p (a:as) =
  if a <= p
    then (a:lt, gt)
    else (lt, a:gt)
  where (lt, gt) = pivot p as

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort (p:as) = lt ++ (p:gt)
  where (lt, gt) = pivot p as

