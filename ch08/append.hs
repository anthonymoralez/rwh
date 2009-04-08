(++) :: [a] -> [a] -> [a] 
(x:xs) ++ ys = x : (xs ++ ys)
[] ++ ys     = ys
