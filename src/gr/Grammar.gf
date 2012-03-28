abstract Grammar = Time ** {

  flags startcat = Question ;

  fun  
    AEAuthorMissing : Answer;
    AENoResults : Answer;
    AENotImplemented : Answer;
    AENoParse : Answer;
    AENoSense : Answer;
    
}