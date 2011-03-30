data Muncher a = Muncher { munch ::  a -> (Maybe a , Muncher a) }


group_by_full 0 max g x  =  (Just g , Muncher (group_by_full max max []))
group_by_full n max g x = (Nothing, Muncher (group_by_full (n-1) max   (g++x)))


group_by n = Muncher ( group_by_full n n [])



sequouille :: Muncher a -> [a] -> [a] -> [a]
sequouille m [] r = r
sequouille m (x:xs) r = case  (munch m x) of 
    (Nothing, m') -> sequouille m' xs  r
    (Just v, m')  -> sequouille m' xs r++[v]
--- # TODO:  write with a fold

