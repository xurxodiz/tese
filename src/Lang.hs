module Lang where
  
import MB
import PGF

import EasterEggs
import LangF
import LangUtils
import Grammar


translate :: String -> IO [String]
translate s = do
  gr <- readPGF "gr/Grammar.pgf"
  let ps = prepareString s
  let ee = easterEgg ps
  xs <- case ee of
            [] -> case parseAllLang gr (startCat gr) ps of
                    [] -> f_noparse gr
                    l -> solve gr l
            e -> return e
  return (map fixString xs)

solve :: PGF -> [(Language,[Tree])] -> IO [String]
solve gr l = do
                  ls <- (sequence . (map $ execute gr) . spread) l
                  return $ reduce $ if any isNotError ls
                                      then filter isNotError ls
                                      else map unerror ls

execute :: PGF -> (Language,Tree) -> IO [String]
execute gr (lg,t) = (f_noresults gr lg) s
  where s = case fg t of
              GTRelease t w -> (f_trelease gr lg) t w
              GTReleaseBy t w a -> (f_treleaseby gr lg) t w a
              GTBorn t a -> (f_tborn gr lg) t a
              GTDie t a -> (f_tdie gr lg) t a
              GTFormed t a -> (f_tformed gr lg) t a
              GTDissolved t a -> (f_tdissolved gr lg) t a
              GTJoin t a z -> (f_tjoin gr lg) t a z
              GTLeave t a z -> (f_tleave gr lg) t a z
              -- _ -> (f_notimplemented gr lg)
