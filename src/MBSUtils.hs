module MBSUtils where
  
import Data.List

-- reverses an ordering
rev :: Ordering -> Ordering
rev GT = LT
rev LT = GT
rev a = a

-- orders dates in string format
-- places 2004-06(-23) before 2004, otherwise, normal order
dateSort :: (Ord a) => [a] -> [a] -> Ordering
dateSort a b = if a `isPrefixOf` b then GT
               else if b `isPrefixOf` a then LT
               else compare a b
              
flip3 :: ((a,b),c) -> (a,(b,c))
flip3 ((a,b),c) = (a,(b,c))
              
-- flattens a 2-tuple into a 3-tuple
flatten3 :: ((a,b),c) -> (a,b,c)
flatten3 ((a,b),c) = (a,b,c) 

flatten3b :: (a,(b,c)) -> (a,b,c)
flatten3b (a,(b,c)) = (a,b,c)

-- flattens a 2-tuple into a 4-tuple
flatten4 :: ((a,b),(c,d)) -> (a,b,c,d) 
flatten4 ((a,b),(c,d)) = (a,b,c,d) 

flatten5 :: ((a,(b,(c,d))),e) -> (a,c,d)
flatten5 ((a,(b,(c,d))),e) = (a,c,d)        
              
-- groups a list of pairs by associating the first element with a list of its seconds
groupByFst :: (Eq a, Eq b) => [(a,b)] -> [(a,[b])] 
groupByFst x = reverse $ assoc [] (map (\(x,y) -> (x,[y])) x)
                where
                  assoc a [] = zip (map fst a) (map (nub . snd) a)
                  assoc a ((h,v):t) = case lookup h a of
                                          Nothing -> assoc ((h,v):a) t
                                          Just b -> let l = deleteBy fstEqual (h,v) a
                                                    in assoc ((h,v++b):l) t
                                      where
                                          fstEqual x y = (fst x) == (fst y)        

-- out of a pair with same first element and several seconds,
-- picks the best second according to the provided function
pickBestSnd :: (b -> b -> Ordering) -> (a,[b]) -> (a,b)
pickBestSnd f (a,[]) = undefined                       -- *shrugs*
pickBestSnd f (a,b) = (a, head $ sortBy f b)