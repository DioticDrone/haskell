-- ======================================================================
stutter :: [a] -> [a]
stutter [] = []
stutter (x:xs) = x:x:(stutter xs)

-- ======================================================================
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then compress xs else x:(compress xs)
                                                                   
-- ======================================================================
select' _ [] _ = []
select' _ _ [] = []
select' pred (x:xs) (y:ys) = if (pred x) then y:rest else rest where rest = select' pred xs ys

findIndices :: (Num b, Enum b) => (a -> Bool) -> [a] -> [b] 
findIndices pred xs = select' pred xs [0..]
                      
delete x xs = filter (/= x) xs

nub' [] = []
nub' (x:xs) = x:(nub' (delete x xs))
                                                                   
intersect :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys = [x | x <- nub' xs, y <- ys, x == y]

-- ======================================================================
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf xs ys = xs == take (length xs) ys
              
-- ======================================================================
isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)
              
-- ======================================================================
dot :: (Num a) => [a] -> [a] -> a
--dot = sum $ zipWith (*)
-- why doesn't ^ work?
dot xs ys = sum $ zipWith (*) xs ys 
            
-- ======================================================================
increasing :: (Ord a) => [a] -> Bool
increasing [x,y] = x < y
increasing (x:xs) = x < head xs && increasing xs
                    
-- ======================================================================
decimate :: [a] -> [a]
decimate = select' (\x -> rem x 10 /= 0) [1..]
           
-- ======================================================================
-- can't figure out how to get this to null terminate
encipher xs [] _ = error "unequal length"
encipher [] ys _ = error "unequal length"
encipher _ _ [] = []
encipher xs ys (l:ls) = (select' (\x -> x == l) xs ys):(encipher xs ys ls)
                        
-- ======================================================================
prefixSum (x:xs) = scanl (+) x (xs)
                   
-- ======================================================================
select _ [] _ = []
select _ _ [] = []
select pred (x:xs) (y:ys)
  | pred x = y:rest
  | otherwise = rest
  where rest = select pred xs ys
                   
-- ======================================================================
  {--
number = foldl addDigit 0
  where addDigit accu d = accu*10 + d
  --}
