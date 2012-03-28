concrete BasicsSpa of Basics = open ResSpa, Prelude in {  

  lincat
  
    Question, Answer,
    AuthorUnitName,
    WorkUnitName ,
    Date, ListDate,
    Token = SS ;
  
    AuthorUnitType,
    WorkUnitType = { s : Str ; g : Genero ; n : Numero } ; 
    
    WorkUnitMod = { s : (Genero*Numero) => Str } ; 
  
    Author = { s : (Preposicion*Determinante) => Str ; g : Genero ; n : Numero } ;
  
    Work = { s : (Preposicion*Determinante) => Str ; g : Genero ; n : Numero } ;

  lin

    WUTSingle = { s = "single"; g = GMasc ; n = NSg } ;
    
    WUTAlbum = variants { { s = "álbum" ; g = GMasc ; n = NSg };
                          { s = "disco" ; g = GMasc ; n = NSg } };
                          
    WUTSong = variants { { s = "canción"; g = GFem ; n = NSg };
                          { s = "tema" ; g = GMasc ; n = NSg } } ;

    WUMFirst = { s = table { <GMasc,NSg> => "primer" ;
                             <GMasc,NPl> => "primeros" ;
                             <GFem,NSg> => "primera" ;
                             <GFem,NPl> => "primeras" } } ;
                            
    WUMLast = { s = table { <GMasc,NSg> => "último" ;
                            <GMasc,NPl> => "últimos" ;
                            <GFem,NSg> => "última" ;
                            <GFem,NPl> => "últimas" } } ;
    WUNName t = t ;

    WNamed t = variants { { s = stdflex (t.s) ! <GMasc,NSg> ; g = GMasc ; n = NSg } ;
                          { s = stdflex (t.s) ! <GFem,NSg> ; g = GFem ; n = NSg } ;
                          { s = stdflex (t.s) ! <GMasc,NPl> ; g = GMasc ; n = NPl } ;
                          { s = stdflex (t.s) ! <GFem,NPl> ; g = GFem ; n = NPl } } ;
                          
    WNamedUnit u n = { s = fzflex (u.s ++ n.s) ! <u.g,u.n> ; g = u.g ; n = u.n } ;
    WModUnit u m = { s = fzflex (m.s ! <u.g,u.n> ++ u.s) ! <u.g,u.n> ; g = u.g ; n = u.n } ;
    WNamedModUnit u m n = { s = fzflex (m.s ! <u.g,u.n> ++ u.s ++ n.s) ! <u.g,u.n> ; g = u.g ; n = u.n } ;
  
    AUTSinger = variants { { s = "cantante" ; g = GMasc ; n = NSg } ;
                           { s = "cantante" ; g = GFem ; n = NSg } } ;
    
    AUTBand = variants { { s = "grupo" ; g = GMasc ; n = NSg } ;
                         { s = "banda" ; g = GFem ; n = NSg } } ; 
    AUNName t = t ;
  
    ANamed t = variants { { s = stdflex (t.s) ! <GMasc,NSg> ; g = GMasc ; n = NSg } ;
                          { s = stdflex (t.s) ! <GMasc,NPl> ; g = GMasc ; n = NPl } ;
                          { s = stdflex (t.s) ! <GFem,NSg> ; g = GFem ; n = NSg } ;
                          { s = stdflex (t.s) ! <GFem,NPl> ; g = GFem ; n = NPl } } ;
    
    ANamedUnit u t = { s = fzflex (u.s ++ t.s) ! <u.g,u.n> ; g = u.g ; n = u.n } ;

    DNamed t = t ;
    BaseDate d = d ;
    ConsDate h t = { s = h.s ++ "," ++ t.s } ;

    TStr t = t ;

}