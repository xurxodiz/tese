module LangR where
  
import PGF

import Grammar
import LangRG
import Types


r_noparse :: PGF -> [String]
r_noparse gr = linearizeAll gr (gf g_noparse)

r_notimplemented :: PGF -> Language -> String
r_notimplemented gr lg = linearize gr lg (gf g_notimplemented)

r_nosense :: PGF -> Language -> String
r_nosense gr lg = linearize gr lg (gf g_nosense)

r_authormissing :: PGF -> Language -> String
r_authormissing gr lg = linearize gr lg (gf g_authormissing)

r_noresults :: PGF -> Language -> String
r_noresults gr lg = linearize gr lg (gf g_noresults)

  
r_treleaseNamed :: PGF -> Language -> (Track, Artist, Date, Album) -> String
-- track may be empty if it's an album
r_treleaseNamed gr lg (w,a,d,x) = case w of
  [] -> r_treleaseAlbum gr lg (x,a,d)
  _ -> r_treleaseSong gr lg (w,a,d,x)

r_treleaseSingle :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleaseSingle gr lg (w,a,d) = 
  let a' = g_namedauthor a
  in let w' = g_namedsingle w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleaseAlbum :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleaseAlbum gr lg (w,a,d) =
  let a' = g_namedauthor a
  in let w' = g_namedalbum w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleaseSong :: PGF -> Language -> (Track, Artist, Date, Album) -> String
r_treleaseSong gr lg (w,a,d,x) =
  let a' = g_namedauthor a
  in let w' = g_namedsong w
  in let x' = g_namedalbum x
  in let d' = g_nameddate d
  in let c = GTAReleaseX a' w' x' d'
  in linearize gr lg (gf c)

r_treleasebyNamed :: PGF -> Language -> (Track, Artist, Date, Album) -> String
-- track may be empty if it's an album
r_treleasebyNamed gr lg (w,a,d,x) = case w of
  [] -> r_treleasebyAlbum gr lg (x,a,d)
  _ -> r_treleasebySong gr lg (w,a,d,x)

r_treleasebySingle :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebySingle = r_treleaseSingle

r_treleasebyAlbum :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebyAlbum = r_treleaseAlbum

r_treleasebySong :: PGF -> Language -> (Track, Artist, Date, Album) -> String
r_treleasebySong = r_treleaseSong

r_treleasebyFirstSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyFirstSingle gr lg (a,w,d) = 
  let a' = g_namedauthor a
  in let w' = g_namedsinglefirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)  

r_treleasebyFirstAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyFirstAlbum gr lg (a,w,d) = 
  let a' = g_namedauthor a
  in let w' = g_namedalbumfirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyFirstSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyFirstSong = r_treleasebyFirstSingle -- semantics, after all

r_treleasebyLastSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyLastSingle gr lg (a,w,d) = 
  let a' = g_namedauthor a
  in let w' = g_namedsinglelast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyLastAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyLastAlbum gr lg (a,w,d) = 
  let a' = g_namedauthor a
  in let w' = g_namedalbumlast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyLastSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyLastSong = r_treleasebyLastSingle -- semantics, after all

r_treleasebyBandNamed :: PGF -> Language -> (Track, Artist, Date, Album) -> String
-- track may be empty if it's an album
r_treleasebyBandNamed gr lg (w,a,d,x) = case w of
  [] -> r_treleasebyBandAlbum gr lg (x,a,d)
  _ -> r_treleasebyBandSong gr lg (w,a,d,x)

r_treleasebyBandSingle :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebyBandSingle gr lg (w,a,d) = 
  let a' = g_namedband a
  in let w' = g_namedsingle w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyBandAlbum :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebyBandAlbum gr lg (w,a,d) =
  let a' = g_namedband a
  in let w' = g_namedalbum w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyBandSong :: PGF -> Language -> (Track, Artist, Date, Album) -> String
r_treleasebyBandSong gr lg (w,a,d,x) =
  let a' = g_namedband a
  in let w' = g_namedsong w
  in let x' = g_namedalbum x
  in let d' = g_nameddate d
  in let c = GTAReleaseX a' w' x' d'
  in linearize gr lg (gf c)

r_treleasebyBandFirstSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandFirstSingle gr lg (a,w,d) = 
  let a' = g_namedband a
  in let w' = g_namedsinglefirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)  

r_treleasebyBandFirstAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandFirstAlbum gr lg (a,w,d) = 
  let a' = g_namedband a
  in let w' = g_namedalbumfirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyBandFirstSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandFirstSong = r_treleasebyBandFirstSingle -- semantics, after all

r_treleasebyBandLastSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandLastSingle gr lg (a,w,d) = 
  let a' = g_namedband a
  in let w' = g_namedsinglelast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyBandLastAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandLastAlbum gr lg (a,w,d) = 
  let a' = g_namedband a
  in let w' = g_namedalbumlast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebyBandLastSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebyBandLastSong = r_treleasebyBandLastSingle -- semantics, after all

r_treleasebySingerNamed :: PGF -> Language -> (Track, Artist, Date, Album) -> String
-- track may be empty if it's an album
r_treleasebySingerNamed gr lg (w,a,d,x) = case w of
  [] -> r_treleasebySingerAlbum gr lg (x,a,d)
  _ -> r_treleasebySingerSong gr lg (w,a,d,x)

r_treleasebySingerSingle :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebySingerSingle gr lg (w,a,d) = 
  let a' = g_namedsinger a
  in let w' = g_namedsingle w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebySingerAlbum :: PGF -> Language -> (Album, Artist, Date) -> String
r_treleasebySingerAlbum gr lg (w,a,d) =
  let a' = g_namedsinger a
  in let w' = g_namedalbum w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebySingerSong :: PGF -> Language -> (Track, Artist, Date, Album) -> String
r_treleasebySingerSong gr lg (w,a,d,x) =
  let a' = g_namedsinger a
  in let w' = g_namedsong w
  in let x' = g_namedalbum x
  in let d' = g_nameddate d
  in let c = GTAReleaseX a' w' x' d'
  in linearize gr lg (gf c)

r_treleasebySingerFirstSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerFirstSingle gr lg (a,w,d) = 
  let a' = g_namedsinger a
  in let w' = g_namedsinglefirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)  

r_treleasebySingerFirstAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerFirstAlbum gr lg (a,w,d) = 
  let a' = g_namedsinger a
  in let w' = g_namedalbumfirst w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebySingerFirstSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerFirstSong = r_treleasebySingerFirstSingle -- semantics, after all

r_treleasebySingerLastSingle :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerLastSingle gr lg (a,w,d) = 
  let a' = g_namedsinger a
  in let w' = g_namedsinglelast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebySingerLastAlbum :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerLastAlbum gr lg (a,w,d) = 
  let a' = g_namedsinger a
  in let w' = g_namedalbumlast w
  in let d' = g_nameddate d
  in let c = GTARelease a' w' d'
  in linearize gr lg (gf c)

r_treleasebySingerLastSong :: PGF -> Language -> (Artist, Album, Date) -> String
r_treleasebySingerLastSong = r_treleasebySingerLastSingle -- semantics, after all

r_tbornNamed :: PGF -> Language -> (Artist, Date) -> String
r_tbornNamed gr lg (a, d) =
  let a' = g_namedauthor a
  in let d' = g_nameddate d
  in let c = GTABorn a' d'
  in linearize gr lg (gf c)
  
r_tbornNamedSinger :: PGF -> Language -> (Artist, Date) -> String
r_tbornNamedSinger gr lg (a, d) =
  let a' = g_namedsinger a
  in let d' = g_nameddate d
  in let c = GTABorn a' d'
  in linearize gr lg (gf c)
  
r_tdieNamed :: PGF -> Language -> (Artist, Date) -> String
r_tdieNamed gr lg (a, d) =
  let a' = g_namedauthor a
  in let d' = g_nameddate d
  in let c = GTADie a' d'
  in linearize gr lg (gf c)
  
r_tdieNamedSinger :: PGF -> Language -> (Artist, Date) -> String
r_tdieNamedSinger gr lg (a, d) =
  let a' = g_namedsinger a
  in let d' = g_nameddate d
  in let c = GTADie a' d'
  in linearize gr lg (gf c)
  
r_tformedNamed :: PGF -> Language -> (Artist, Date) -> String
r_tformedNamed gr lg (a, d) =
  let a' = g_namedband a
  in let d' = g_nameddate d
  in let c = GTAFormed a' d'
  in linearize gr lg (gf c)

r_tformedNamedBand :: PGF -> Language -> (Artist, Date) -> String
r_tformedNamedBand = r_tformedNamed

r_tdissolvedNamed :: PGF -> Language -> (Artist, Date) -> String
r_tdissolvedNamed gr lg (a, d) =
  let a' = g_namedband a
  in let d' = g_nameddate d
  in let c = GTADissolved a' d'
  in linearize gr lg (gf c)

r_tdissolvedNamedBand :: PGF -> Language -> (Artist, Date) -> String
r_tdissolvedNamedBand = r_tdissolvedNamed
  
r_tjoin :: PGF -> Language -> (Artist, Artist, [Date]) -> String
r_tjoin gr lg (a,z,d) =
  let a' = g_namedauthor a
  in let z' = g_namedband z
  in let d' = g_listdate d
  in let c = GTAJoin a' z' d'
  in linearize gr lg (gf c)
  
r_tleave :: PGF -> Language -> (Artist, Artist, [Date]) -> String
r_tleave gr lg (a,z,d) =
  let a' = g_namedauthor a
  in let z' = g_namedband z
  in let d' = g_listdate d
  in let c = GTALeave a' z' d'
  in linearize gr lg (gf c)