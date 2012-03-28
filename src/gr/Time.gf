abstract Time = Basics ** {

  cat
    TimeSought ;

  fun

    -- questions

    TRelease : TimeSought -> Work -> Question ;
    TReleaseBy : TimeSought -> Work -> Author -> Question ;
    
    TBorn, TFormed, TDie, TDissolved : TimeSought -> Author -> Question ;
    TJoin, TLeave : TimeSought -> Author -> Author -> Question ;
    
    TSWhen, TSYear : TimeSought ;
    
    -- answers
    
    TARelease: Author -> Work -> Date -> Answer;
    TAReleaseX : Author -> Work -> Work -> Date -> Answer;
    
    TABorn, TADie : Author -> Date -> Answer;
    TAFormed, TADissolved : Author -> Date -> Answer;
    TAJoin, TALeave : Author -> Author -> ListDate -> Answer ;
    
}