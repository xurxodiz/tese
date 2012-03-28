abstract Basics = {

  cat
    Question ; Answer ;
    Work ; WorkUnitType ; WorkUnitMod ; WorkUnitName ;
    Author ; AuthorUnitType ; AuthorUnitName ;
    Date ; ListDate ;
    Token ;
    
  fun
  
    WNamed : Token -> Work ;
    WModUnit : WorkUnitType -> WorkUnitMod -> Work ;
    WNamedUnit : WorkUnitType -> WorkUnitName -> Work ;
    WNamedModUnit : WorkUnitType -> WorkUnitMod -> WorkUnitName -> Work ;
    
    ANamed : Token -> Author ;
    ANamedUnit : AuthorUnitType -> AuthorUnitName -> Author ;

    WUTSingle, WUTAlbum, WUTSong : WorkUnitType ;
    WUMFirst, WUMLast : WorkUnitMod ;
    WUNName : Token -> WorkUnitName ;

    AUTSinger, AUTBand : AuthorUnitType ;
    AUNName : Token -> AuthorUnitName ;
  
    DNamed : Token -> Date ;
    BaseDate : Date -> ListDate ;
    ConsDate : Date -> ListDate -> ListDate ;
  
    TStr : String -> Token ;
}