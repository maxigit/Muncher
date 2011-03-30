data Muncher a = Muncher { munch ::  a -> (Maybe a , Muncher a) }


group_by 0 max g x  =  (Just g , Muncher (group_by max max []))
group_by n max g x = (Nothing, Muncher (group_by (n-1) max   (g++x)))

