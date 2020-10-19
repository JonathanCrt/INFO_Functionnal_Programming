module MergeSort where

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort pred [] = []
mergesort pred [x] = [x]
mergesort pred xs = merge pred xs1' xs2' --- We merge the two sub-parts to find the starting data
  where
  (xs1, xs2) = split xs --- We cut the data to be sorted into two more or less equal parts
  xs1' = mergesort pred xs1 -- we sort first part
  xs2' = mergesort pred xs2 -- we sort second part

split :: [a] -> ([a],[a]) 
split xs = go xs xs 
  where
go :: [a1] -> [a2] -> ([a1], [a1])
go (x : xs) (_:_:zs) = (x : us, vs) 
  where (us, vs) = go xs zs
go xs _ = ([], xs)

--- pred = predecessor of value
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge pred xs [] = xs
merge pred [] ys = ys
merge pred (x:xs) (y:ys)
    | pred x y  = x : merge pred xs (y : ys)
    | otherwise = y : merge pred (x : xs) ys
    

