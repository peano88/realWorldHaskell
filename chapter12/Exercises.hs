tElem' :: (a,a,a) -> Int -> a
tElem' (x,_,_) 0 = x
tElem' (_,y,_) 1 = y
tElem' (_,_,z) 2 = z
tElem' _ _ = error "Fucked up index"

tElem :: (a,a,a,a) -> Int -> a
tElem (x,_,_,_) 0 = x
tElem (_,y,z,w) n = tElem' (y,z,w) (n-1)

tElem5 :: (a,a,a,a,a) -> Int -> a
tElem5 (x,_,_,_,_) 0 = x
tElem5 (_,y,z,w,t) n = tElem (y,z,w,t) (n-1)

tElem6 :: (a,a,a,a,a,a) -> Int -> a
tElem6 (x,_,_,_,_,_) 0 = x
tElem6 (_,y,z,w,t,u) n = tElem5 (y,z,w,t,u) (n-1)