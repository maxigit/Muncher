data Muncher a b = Muncher { munch ::  (Maybe a) -> ( [b]  , Maybe (Muncher a b)) }
-- 

--build_muncher :: (a -> Either b (Muncher b b))  ->  Muncher a b

group_by_ _ group Nothing = (group, Nothing)  -- job finished, muncher die
group_by_ 0 group _ = (group,  Nothing)
group_by_ n group (Just x) = ([], Just (Muncher (group_by_ (n-1) (group++[x]))))

group_by n = Muncher ( group_by_ n [])



sequouille_ :: Muncher a b ->  [b] -> [a] -> [b]
sequouille_ m r [] = r++vs where (vs, _) =  munch m Nothing
sequouille_ m r (x:xs)  = case  (munch m (Just x)) of 
    (vs, Nothing) -> r ++ vs
    (vs, Just  m')  -> sequouille_ m' (r++vs) xs 
--- # TODO:  write with a fold
--
munching muncher = sequouille_ muncher []

