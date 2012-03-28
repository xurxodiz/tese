module LangF where
  
import MB
import PGF
  
import LangR
import LangUtils
import Grammar
  
f_noparse :: PGF -> IO [String]
f_noparse gr = return (r_noparse gr)

f_notimplemented :: PGF -> Language -> IO [String]
f_notimplemented gr lg = return [errorHeader, r_notimplemented gr lg]

f_nosense :: PGF -> Language -> IO [String]
f_nosense gr lg = return [errorHeader, r_nosense gr lg]

f_authormissing :: PGF -> Language -> IO [String]
f_authormissing gr lg = return [errorHeader, r_authormissing gr lg]

f_noresults :: PGF -> Language -> IO [String] -> IO [String]
f_noresults gr lg l = do
                      x <- l
                      case x of
                        [] -> return [errorHeader, r_noresults gr lg]
                        _ -> return x


f_trelease :: PGF -> Language -> GTimeSought -> GWork -> IO [String]
f_trelease gr lg t w = case w of
                  GWNamed (GTStr (GString n)) -> pipe n a_treleaseNamed (r_treleaseNamed gr lg)
                  GWNamedUnit ty (GWUNName (GTStr (GString n)))
                    -> case ty of
                      GWUTSingle -> pipe n a_treleaseSingle (r_treleaseSingle gr lg)
                      GWUTAlbum  -> pipe n a_treleaseAlbum (r_treleaseAlbum gr lg)
                      GWUTSong   -> pipe n a_treleaseSong (r_treleaseSong gr lg)
                  GWModUnit _ _ -> f_authormissing gr lg
                  GWNamedModUnit _ _ _ -> f_authormissing gr lg
                    
                    
f_treleaseby :: PGF -> Language -> GTimeSought -> GWork -> GAuthor -> IO [String]
f_treleaseby gr lg t w a = case a of
  
                    GANamed (GTStr (GString as))
                      -> case w of
                        GWNamed (GTStr (GString n)) -> pipe (n,as) a_treleasebyNamed (r_treleasebyNamed gr lg)
                        GWModUnit ty pos -> case pos of
                          GWUMFirst -> case ty of
                            GWUTSingle -> pipe as a_treleasebyFirstSingle (r_treleasebyFirstSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebyFirstAlbum (r_treleasebyFirstAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebyFirstSong (r_treleasebyFirstSong gr lg)
                          GWUMLast -> case ty of
                            GWUTSingle -> pipe as a_treleasebyLastSingle (r_treleasebyLastSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebyLastAlbum (r_treleasebyLastAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebyLastSong (r_treleasebyLastSong gr lg)
                        GWNamedUnit ty (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebySingle (r_treleasebySingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebyAlbum (r_treleasebyAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebySong (r_treleasebySong gr lg)
                        GWNamedModUnit ty _ (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebySingle (r_treleasebySingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebyAlbum (r_treleasebyAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebySong (r_treleasebySong gr lg)  
    
                    GANamedUnit GAUTBand (GAUNName (GTStr (GString as)))
                      -> case w of
                        GWNamed (GTStr (GString n)) -> pipe (n,as) a_treleasebyBandNamed (r_treleasebyBandNamed gr lg)
                        GWModUnit ty pos -> case pos of
                          GWUMFirst -> case ty of
                            GWUTSingle -> pipe as a_treleasebyBandFirstSingle (r_treleasebyBandFirstSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebyBandFirstAlbum (r_treleasebyBandFirstAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebyBandFirstSong (r_treleasebyBandFirstSong gr lg)
                          GWUMLast -> case ty of
                            GWUTSingle -> pipe as a_treleasebyBandLastSingle (r_treleasebyBandLastSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebyBandLastAlbum (r_treleasebyBandLastAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebyBandLastSong (r_treleasebyBandLastSong gr lg)
                        GWNamedUnit ty (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebyBandSingle (r_treleasebyBandSingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebyBandAlbum (r_treleasebyBandAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebyBandSong (r_treleasebyBandSong gr lg)
                        GWNamedModUnit ty _ (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebyBandSingle (r_treleasebyBandSingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebyBandAlbum (r_treleasebyBandAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebyBandSong (r_treleasebyBandSong gr lg)
    
                    GANamedUnit GAUTSinger (GAUNName (GTStr (GString as)))
                      -> case w of
                        GWNamed (GTStr (GString n)) -> pipe (n,as) a_treleasebySingerNamed (r_treleasebySingerNamed gr lg)
                        GWModUnit ty pos -> case pos of
                          GWUMFirst -> case ty of
                            GWUTSingle -> pipe as a_treleasebySingerFirstSingle (r_treleasebySingerFirstSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebySingerFirstAlbum (r_treleasebySingerFirstAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebySingerFirstSong (r_treleasebySingerFirstSong gr lg)
                          GWUMLast -> case ty of
                            GWUTSingle -> pipe as a_treleasebySingerLastSingle (r_treleasebySingerLastSingle gr lg)
                            GWUTAlbum  -> pipe as a_treleasebySingerLastAlbum (r_treleasebySingerLastAlbum gr lg)
                            GWUTSong   -> pipe as a_treleasebySingerLastSong (r_treleasebySingerLastSong gr lg)
                        GWNamedUnit ty (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebySingerSingle (r_treleasebySingerSingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebySingerAlbum (r_treleasebySingerAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebySingerSong (r_treleasebySingerSong gr lg)
                        GWNamedModUnit ty _ (GWUNName (GTStr (GString n)))
                          -> case ty of
                            GWUTSingle -> pipe (n,as) a_treleasebySingerSingle (r_treleasebySingerSingle gr lg)
                            GWUTAlbum  -> pipe (n,as) a_treleasebySingerAlbum (r_treleasebySingerAlbum gr lg)
                            GWUTSong   -> pipe (n,as) a_treleasebySingerSong (r_treleasebySingerSong gr lg)
                        
f_tborn :: PGF -> Language -> GTimeSought -> GAuthor -> IO [String]
f_tborn gr lg t a = case a of
                      GANamed (GTStr (GString n)) -> pipe n a_tbornNamed (r_tbornNamed gr lg)
                      GANamedUnit ty (GAUNName (GTStr (GString n))) -> case ty of
                        GAUTSinger -> pipe n a_tbornNamedSinger (r_tbornNamedSinger gr lg)
                        GAUTBand -> f_nosense gr lg
                      
f_tdie :: PGF -> Language -> GTimeSought -> GAuthor -> IO [String]
f_tdie gr lg t a = case a of
                      GANamed (GTStr (GString n)) -> pipe n a_tdieNamed (r_tdieNamed gr lg)
                      GANamedUnit ty (GAUNName (GTStr (GString n))) -> case ty of
                        GAUTSinger -> pipe n a_tdieNamedSinger (r_tdieNamedSinger gr lg)
                        GAUTBand -> f_nosense gr lg
                        
f_tformed :: PGF -> Language -> GTimeSought -> GAuthor -> IO [String]
f_tformed gr lg t a = case a of
                      GANamed (GTStr (GString n)) -> pipe n a_tformedNamed (r_tformedNamed gr lg)
                      GANamedUnit ty (GAUNName (GTStr (GString n))) -> case ty of
                        GAUTBand -> pipe n a_tformedNamedBand (r_tformedNamedBand gr lg)
                        GAUTSinger -> f_nosense gr lg

f_tdissolved :: PGF -> Language -> GTimeSought -> GAuthor -> IO [String]
f_tdissolved gr lg t a = case a of
                      GANamed (GTStr (GString n)) -> pipe n a_tdissolvedNamed (r_tdissolvedNamed gr lg)
                      GANamedUnit ty (GAUNName (GTStr (GString n))) -> case ty of
                        GAUTBand -> pipe n a_tdissolvedNamedBand (r_tdissolvedNamedBand gr lg)
                        GAUTSinger -> f_nosense gr lg
                        
f_tjoin :: PGF -> Language -> GTimeSought -> GAuthor -> GAuthor -> IO [String]
f_tjoin gr lg t a z = case (a,z) of
    (GANamedUnit GAUTBand _, _) -> f_nosense gr lg
    (_, GANamedUnit GAUTSinger _) -> f_nosense gr lg
    _ -> let a' = case a of
              GANamed (GTStr (GString n)) -> n
              GANamedUnit GAUTSinger (GAUNName (GTStr (GString n))) -> n
          in case z of 
              GANamed (GTStr (GString n)) -> pipe (a',n) a_tjoin (r_tjoin gr lg)
              GANamedUnit GAUTBand (GAUNName (GTStr (GString n))) -> pipe (a',n) a_tjoin (r_tjoin gr lg)
          
f_tleave :: PGF -> Language -> GTimeSought -> GAuthor -> GAuthor -> IO [String]
f_tleave gr lg t a z = case (a,z) of
    (GANamedUnit GAUTBand _, _) -> f_nosense gr lg
    (_, GANamedUnit GAUTSinger _) -> f_nosense gr lg
    _ -> let a' = case a of
              GANamed (GTStr (GString n)) -> n
              GANamedUnit GAUTSinger (GAUNName (GTStr (GString n))) -> n
          in case z of 
              GANamed (GTStr (GString n)) -> pipe (a',n) a_tleave (r_tleave gr lg)
              GANamedUnit GAUTBand (GAUNName (GTStr (GString n))) -> pipe (a',n) a_tleave (r_tleave gr lg)
