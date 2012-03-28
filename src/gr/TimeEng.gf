concrete TimeEng of Time = BasicsEng ** open Prelude, ResEng in {

  lincat
    TimeSought = SS;
    
  lin

    -- questions
  
    TRelease t w = let time = t.s in
                   let wa = w.s ! DArt ! NSg  in
                   { s = time ++ variants {
                                 "did" ++ wa ++ "come out";
                                  "was" ++ wa ++ "released" } };
              
    TReleaseBy t w a = let time = t.s in
                       let attrib = variants { "by"; "of" } in
                       let wa = w.s ! DArt ! a.n in
                       let wp = variants { w.s ! DPos ! a.n ; w.s ! DArt ! a.n } in
                       let a = variants { a.s ! AThe ; a.s ! ANone } in
                       { s = time ++  variants {
                                      "did" ++ wa ++ attrib ++ a ++ "come out";
                                      "was" ++ wa ++ "released by" ++ a;
                                      "was" ++ wa ++ attrib ++ a ++ "released";
                                      "did" ++ a ++ "release" ++ wp ; } };
                                    
    TBorn t a = let time = t.s in
                let as = variants { a.s ! AThe ; a.s ! ANone } in
                { s = time ++ "was" ++ as ++ "born"; };
              
    TFormed t a = let time = t.s in
                    let as = variants { a.s ! AThe ; a.s ! ANone } in
                    { s = time ++ variants {"was";"were"} ++ as ++ variants{"formed";"founded";"created"}; };
              
    TDie t a = let time = t.s in
               let as = variants { a.s ! AThe ; a.s ! ANone } in
               { s = time ++ "did" ++ as ++ "die"; };
              
    TDissolved t a = let time = t.s in
                      let as = variants { a.s ! AThe ; a.s ! ANone } in
                       { s = time ++ variants {"was";"were"} ++ as ++ "dissolved"; };
          
    TJoin t a z = let time = t.s in
                  let as = variants { a.s ! AThe ; a.s ! ANone } in
                   let zs = variants { z.s ! AThe ; z.s ! ANone } in
                   { s = time ++ "did" ++ as ++ "join" ++ zs; };
              
    TLeave t a z = let time = t.s in
                   let as = variants { a.s ! AThe ; a.s ! ANone } in
                   let zs = variants { z.s ! AThe ; z.s ! ANone } in
                     { s = time ++ "did" ++ as ++ "leave" ++ zs; };
          
    TSWhen = ss "when";
    TSYear = ss "in what year" ;
    
    
    -- answers
    
    
    TARelease a w d = { s = a.s ! AThe ++ "released" ++ w.s ! DArt ! a.n ++ "in" ++ d.s };
    TAReleaseX a w x d = { s = a.s ! AThe ++ "released" ++ w.s ! DArt ! a.n ++
                              "(from" ++ x.s ! DArt ! a.n ++ ")" ++ "in" ++ d.s };
  
    TABorn a d = { s = a.s ! AThe ++ "was born in" ++ d.s };
    TADie a d = { s = a.s ! AThe ++ "died in" ++ d.s };
  
    TAFormed a d = { s = a.s ! AThe ++ "was formed in" ++ d.s };
    TADissolved a d = { s = a.s ! AThe ++ "was dissolved in" ++ d.s };
  
    TAJoin a b d = { s = a.s ! AThe ++ "joined" ++ b.s ! AThe ++ "in" ++ d.s };
    TALeave a b d = { s = a.s ! AThe ++ "left" ++ b.s ! AThe ++ "in" ++ d.s };
}