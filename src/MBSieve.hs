module MBSieve where
  
import Codec.Binary.UTF8.String
import Data.List
  
import MBSUtils

--
-- utility sieves
--

s_1 :: [String] -> [String]
s_1 = take 3 . map encodeString

s_2 :: [(String,String)] -> [(String,String)]
s_2 = take 3 . map (\(a,b) -> (encodeString a, encodeString b))

s_3 :: [(String,String,String)] -> [(String,String,String)]
s_3 = take 3 . map (\(a,b,c) -> (encodeString a, encodeString b, encodeString c))

s_4 :: [(String,String,String,String)] -> [(String,String,String,String)]
s_4 = take 3 . map (\(a,b,c,d) -> (encodeString a, encodeString b, encodeString c, encodeString d))

s_expand34 :: [(a,b,c)] -> [([d],b,c,a)]
s_expand34 = map (\(a,b,c) -> ([],b,c,a))

--
-- individual sieves for the arrows
--

s_treleaseNamed :: [(String,String,String,String)] -> [(String,String,String,String)]
s_treleaseNamed = id -- it's a concatenation of arrows, so they are already sieved

s_treleaseSingle :: [((String,String),String)] -> [(String,String,String)]
s_treleaseSingle l = s_3 (map (flatten3 . pickBestSnd dateSort) (groupByFst l))  

s_treleaseAlbum :: [((String,String),String)] -> [(String,String,String)]
s_treleaseAlbum = s_treleaseSingle

s_treleaseSong :: [((String,String),(String,String))] -> [(String,String,String,String)]
s_treleaseSong l =  s_4 (map (flatten4 . pickBestSnd mySort) (groupByFst l))
                    where
                      mySort a b = dateSort (fst a) (fst b)
      
s_treleasebyNamed :: [(String,String,String,String)] -> [(String,String,String,String)]                
s_treleasebyNamed = s_treleaseNamed

s_treleasebySingle :: [((String,String),String)] -> [(String,String,String)]
s_treleasebySingle = s_treleaseSingle

s_treleasebyAlbum :: [((String,String),String)] -> [(String,String,String)]
s_treleasebyAlbum = s_treleaseAlbum

s_treleasebySong :: [((String,String),(String,String))] -> [(String,String,String,String)]
s_treleasebySong = s_treleaseSong

s_treleasebyFirstSingle :: [(String,(String,String))] -> [(String,String,String)]
s_treleasebyFirstSingle l = s_3 (map (flatten3b . pickBestSnd mySort) (groupByFst l))    
                              where
                                mySort a b = dateSort (snd a) (snd b)
                                
s_treleasebyFirstAlbum :: [(String,(String,String))] -> [(String,String,String)]
s_treleasebyFirstAlbum = s_treleasebyFirstSingle

-- first we pick the earliest release date for each single
-- then, out of those, we pick the latest
s_treleasebyLastSingle :: [((String,String),String)] -> [(String,String,String)]
s_treleasebyLastSingle l = let x = map (flip3 . pickBestSnd dateSort) $ groupByFst l
                           in s_3 (map (flatten3b . pickBestSnd mySort) (groupByFst x))  
                            where
                              mySort a b = rev $ dateSort (snd a) (snd b)

s_treleasebyLastAlbum :: [((String,String),String)] -> [(String,String,String)]
s_treleasebyLastAlbum = s_treleasebyLastSingle

s_tbornNamed :: [(String,String)] -> [(String,String)]
s_tbornNamed = s_2

s_tdieNamed :: [(String,String)] -> [(String,String)]
s_tdieNamed = s_2

s_tformedNamed :: [(String,String)] -> [(String,String)]
s_tformedNamed = s_2

s_tdissolvedNamed :: [(String,String)] -> [(String,String)]
s_tdissolvedNamed = s_2
            
s_tjoin :: [(String,(String,(String,(String,String))))] -> [(String,String,[String])]
s_tjoin l = let z =  groupByFst [((d,b),e) | (a,(b,(c,(d,e)))) <- l, a == c]
            in let (x,y) = (map fst z, map snd z)
            in let y' = map (sortBy dateSort) y
            in map flatten3 $ zip (s_2 x) (map s_1 y')

s_tleave :: [(String,(String,(String,(String,String))))] -> [(String,String,[String])]            
s_tleave = s_tjoin