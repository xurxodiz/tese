concrete BasicsGal of Basics = open ResGal, Prelude in {  

  lincat
  
    Question, Answer,
    AuthorUnitName,
    WorkUnitName ,
    Date, ListDate,
    Token = SS ;
  
    AuthorUnitType,
    WorkUnitType = { s : Str ; x : Xenero ; n : Numero } ; 
    
    WorkUnitMod = { s : (Xenero*Numero) => Str } ; 
  
    Author = { s : (Preposicion*Determinante) => Str ; x : Xenero ; n : Numero } ;
  
    Work = { s : (Preposicion*Determinante) => Str ; x : Xenero ; n : Numero } ;

  lin

    WUTSingle = { s = "single"; x = XMasc ; n = NSg } ;
    
    WUTAlbum = variants { { s = "álbum" ; x = XMasc ; n = NSg };
                          { s = "disco" ; x = XMasc ; n = NSg } };
                          
    WUTSong = variants { { s = "canción"; x = XFem ; n = NSg };
                         { s = "canzón"; x = XFem ; n = NSg };
                         { s = "tema" ; x = XMasc ; n = NSg } } ;

    WUMFirst = { s = table { <XMasc,NSg> => "primeiro" ;
                             <XMasc,NPl> => "primeiros" ;
                             <XFem,NSg> => "primeira" ;
                             <XFem,NPl> => "primeiras" } } ;
                            
    WUMLast = { s = table { <XMasc,NSg> => variants{"último";"derradeiro"} ;
                            <XMasc,NPl> => variants{"últimos";"derradeiros"} ;
                            <XFem,NSg> => variants{"última";"derradeira"} ;
                            <XFem,NPl> => variants{"últimas";"derradeiras"} } } ;
    WUNName t = t ;

    WNamed t = variants { { s = stdflex (t.s) ! <XMasc,NSg> ; x = XMasc ; n = NSg } ;
                          { s = stdflex (t.s) ! <XFem,NSg> ; x = XFem ; n = NSg } ;
                          { s = stdflex (t.s) ! <XMasc,NPl> ; x = XMasc ; n = NPl } ;
                          { s = stdflex (t.s) ! <XFem,NPl> ; x = XFem ; n = NPl } } ;
                          
    WNamedUnit u n = { s = fzflex (u.s ++ n.s) ! <u.x,u.n> ; x = u.x ; n = u.n } ;
    WModUnit u m = { s = fzflex (m.s ! <u.x,u.n> ++ u.s) ! <u.x,u.n> ; x = u.x ; n = u.n } ;
    WNamedModUnit u m n = { s = fzflex (m.s ! <u.x,u.n> ++ u.s ++ n.s) ! <u.x,u.n> ; x = u.x ; n = u.n } ;
  
    AUTSinger = variants { { s = "cantante" ; x = XMasc ; n = NSg } ;
                           { s = "cantante" ; x = XFem ; n = NSg } } ;
    
    AUTBand = variants { { s = "grupo" ; x = XMasc ; n = NSg } ;
                         { s = "banda" ; x = XFem ; n = NSg } } ; 
    AUNName t = t ;
  
    ANamed t = variants { { s = stdflex (t.s) ! <XMasc,NSg> ; x = XMasc ; n = NSg } ;
                          { s = stdflex (t.s) ! <XMasc,NPl> ; x = XMasc ; n = NPl } ;
                          { s = stdflex (t.s) ! <XFem,NSg> ; x = XFem ; n = NSg } ;
                          { s = stdflex (t.s) ! <XFem,NPl> ; x = XFem ; n = NPl } } ;
    
    ANamedUnit u t = { s = fzflex (u.s ++ t.s) ! <u.x,u.n> ; x = u.x ; n = u.n } ;

    DNamed t = t ;
    BaseDate d = d ;
    ConsDate h t = { s = h.s ++ "," ++ t.s } ;

    TStr t = t ;

}