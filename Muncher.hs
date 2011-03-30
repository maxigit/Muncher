data Muncher a b = Muncher { munch ::  a -> (Maybe b  , Muncher a b) }
-- 

build_muncher :: (a -> Either b (Muncher a b))  ->  Muncher a b

group_by_full 1 max g x  =  (Just g , Muncher (group_by_full max max [x]))
group_by_full n max g x = (Nothing, Muncher (group_by_full (n-1) max   (g++[x])))


group_by n = Muncher ( group_by_full n n [])



sequouille :: Muncher a b ->  [a] -> [b] -> [b]
sequouille m [] r = r
sequouille m (x:xs) r = case  (munch m x) of 
    (Nothing, m') -> sequouille m' xs  r
    (Just v, m')  -> sequouille m' xs (r++[v])
--- # TODO:  write with a fold

