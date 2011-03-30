data Muncher a = Muncher { munch ::  a -> (Just a , Muncher a) }
