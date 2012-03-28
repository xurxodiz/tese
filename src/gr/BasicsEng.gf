concrete BasicsEng of Basics = open ResEng, Prelude in {

  lincat
  
    Question, Answer,
    WorkUnitType , WorkUnitMod , WorkUnitName ,
    AuthorUnitName,
    Date, ListDate,
    Token = SS ;
    
    AuthorUnitType = { s : Str ; n : Number } ;
  
    Author = { s : Article => Str ; n : Number } ;
  
    Work = { s : Determiner => Number => Str } ;

  lin

    WUTSingle = ss "single" ;
    WUTAlbum = ss "album" ;
    WUTSong = ss "song" ;

    WUMFirst = ss "first" ;
    WUMLast = variants { ss "last" ; ss "latest" } ;
    WUNName t = t ;

    WNamed t = { s = table { _ => table { _ => t.s } } };
    WNamedUnit u n = { s = detflex (u.s ++ n.s) } ;
    WModUnit u m = { s = detflex (m.s ++ u.s) } ;
    WNamedModUnit u m n = { s = detflex (m.s ++ u.s ++ n.s) } ;
  
    AUTSinger = { s = "singer" ; n = NSg } ;
    AUTBand = { s = variants {"band"; "group" } ; n = NPl } ; 
    AUNName t = t ;
  
    ANamed t = { s = table { _ => t.s } ; n = NUk };
    ANamedUnit u t = { s = artflex (u.s ++ t.s) ; n = u.n } ;

    DNamed t = t ;
    BaseDate d = d ;
    ConsDate h t = { s = h.s ++ "," ++ t.s } ;

    TStr t = t ;

}