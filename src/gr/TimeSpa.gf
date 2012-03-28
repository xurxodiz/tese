concrete TimeSpa of Time = BasicsSpa ** open Prelude, ResSpa in {

  lincat
    TimeSought = SS;

  lin
  
    -- questions
  
    TRelease t w = let ts = t.s in
                   let sl = table { NSg => "salió" ; NPl => "salieron" } ! w.n in
                   let lz = table { NSg => "se lanzó" ; NPl => "se lanzaron" } ! w.n in
                   let ws = variants { w.s ! <PNo,DNo> ; w.s ! <PNo,DEl> } in
                   { s = ts ++ variants { sl ++ ws;
                                            lz ++ ws;
                                            "sacaron" ++ ws } };
              
    TReleaseBy t w a = let ts = t.s in
                       let sc = table { NSg => "sacó" ; NPl => "sacaron" } ! a.n in
                       let lz = table { NSg => "lanzó" ; NPl => "lanzaron" } ! a.n in
                       let ws = variants { w.s ! <PNo,DNo> ; w.s ! <PNo,DEl> } in
                       let wsp = variants { w.s ! <PNo,DPos> ; w.s ! <PNo,DEl> ; w.s ! <PNo,DNo> } in
                       let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> } in
                       let asd = variants { a.s ! <PDe,DNo> ; a.s ! <PDe,DEl> } in
                       { s = ts ++ variants { "salió" ++ ws ++ asd;
                                               sc ++ variants { as ++ wsp;
                                                                wsp ++ as };
                                               lz ++ variants { as ++ wsp;
                                                                 wsp ++ as } } };
                                    
    TBorn t a = let ts = t.s in
                let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
                { s = ts ++ "nació" ++ as; };
              
    TFormed t a = let ts = t.s in
                    let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
                   let f = table { NSg => variants {"se formó"; "se fundó"; "se creó" };
                                   NPl => variants {"se formaron"; "se fundaron"; "se crearon" } }
                            ! a.n in
                    { s = ts ++ f ++ as; };
              
    TDie t a = let ts = t.s in
               let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
               { s = ts ++ "murió" ++ as; };
              
    TDissolved t a = let ts = t.s in
                      let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
                     let d = table { NSg => variants { "se separó"; "se disolvió" };
                                      NPl => variants { "se separaron"; "se disolvieron" } } ! a.n
                    in { s = ts ++ d ++ as; };
          
    TJoin t a z = let ts = t.s in
                  let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
                  let za = variants { z.s ! <PAa,DEl> ; z.s ! <PAa,DNo> } in
                  let ze = variants { z.s ! <PEn,DEl> ; z.s ! <PEn,DNo> } in
                   { s = ts ++ variants { "se unió" ++ variants { as ++ za ;
                                                                 za ++ as };
                                         "entró" ++ variants { as ++ ze ;
                                                                ze ++ as } } };
              
    TLeave t a z = let ts = t.s in
                  let as = variants { a.s ! <PNo,DEl> ; a.s ! <PNo,DNo> } in
                  let zs = variants { z.s ! <PNo,DEl> ; z.s ! <PNo,DNo> ;
                                      z.s ! <PAa,DEl> ; z.s ! <PAa,DNo> } in
                  let zd = variants { z.s ! <PDe,DEl> ; z.s ! <PDe,DNo> } in
                   { s = ts ++ variants { "salió" ++ variants { as ++ zd ;
                                                               zd ++ as };
                                         "se fue" ++ variants { as ++ zd ;
                                                                zd ++ as };
                                         "dejó" ++ variants { as ++ zs ;
                                                              zs ++ as } } };
          
    TSWhen = ss "cuándo";
    TSYear = ss "en qué año" ;
    
    
    -- answers
    
    
    TARelease a w d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> }
                     in let lz = table { NSg => "lanzó" ; NPl => "lanzaron" } ! a.n
                     in let ws = variants { w.s ! <PNo,DPos> ; w.s ! <PNo,DEl> }
                      in { s = as ++ lz ++ ws ++ "en" ++ d.s };
                      
    TAReleaseX a w x d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> }
                        in let lz = table { NSg => "lanzó" ; NPl => "lanzaron" } ! a.n
                        in let ws = variants { w.s ! <PNo,DEl> ; w.s ! <PNo,DNo> ; w.s ! <PNo,DPos> }
                        in let xs = variants { x.s ! <PDe,DPos> ; x.s ! <PDe,DNo> ; x.s ! <PDe,DEl> }
                        in  { s = as ++ lz ++ ws ++ "(" ++ xs ++ ")" ++ "en" ++ d.s };
  
    TABorn a d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> }
                in { s = as ++ "nació en" ++ d.s };
                
    TADie a d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> }
                in { s = as ++ "murió en" ++ d.s };
  
    TAFormed a d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> } 
                  in let f = table { NSg => "se formó" ; NPl => "se formaron" } ! a.n
                  in { s = as ++ f ++ "en" ++ d.s };
                  
    TADissolved a d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> }
                      in let f = table { NSg => "se disolvió" ; NPl => "se separaron" } ! a.n
                      in { s = as ++ f ++ "en" ++ d.s };
  
    TAJoin a b d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> } 
                  in let bs = variants { b.s ! <PAa,DNo> ; b.s ! <PAa,DEl> } 
                  in { s = as ++ "se unió" ++ bs ++ "en" ++ d.s };
                  
    TALeave a b d = let as = variants { a.s ! <PNo,DNo> ; a.s ! <PNo,DEl> } 
                    in let bs = variants { b.s ! <PAa,DNo> ; b.s ! <PAa,DEl> ;
                                           b.s ! <PNo,DNo> ; b.s ! <PNo,DEl> } 
                    in { s = as ++ "dejó" ++ bs ++ "en" ++ d.s };
}