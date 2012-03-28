concrete TimeGal of Time = BasicsGal ** open Prelude, ResGal in {

  lincat
    TimeSought = SS;

  lin
  
    -- questions
  
    TRelease t w = let ts = t.s in
                   let sl = table { NSg => "saiu" ; NPl => "saíron" } ! w.n in
                   let lz = table { NSg => "se lanzou" ; NPl => "se lanzaron" } ! w.n in
                   let ws = variants { w.s ! <PNon,DNon> ; w.s ! <PNon,DOo> } in
                   { s = ts ++ variants { sl ++ ws;
                                            lz ++ ws;
                                            "sacaron" ++ ws } };
              
    TReleaseBy t w a = let ts = t.s in
                       let sc = table { NSg => "sacou" ; NPl => "sacaron" } ! a.n in
                       let lz = table { NSg => "lanzou" ; NPl => "lanzaron" } ! a.n in
                       let ws = variants { w.s ! <PNon,DNon> ; w.s ! <PNon,DOo> } in
                       let wsp = variants { w.s ! <PNon,DPos> ; w.s ! <PNon,DOo> ; w.s ! <PNon,DNon> } in
                       let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> } in
                       let asd = variants { a.s ! <PDe,DNon> ; a.s ! <PDe,DOo> } in
                       { s = ts ++ variants { "saiu" ++ ws ++ asd;
                                               sc ++ variants { as ++ wsp;
                                                                wsp ++ as };
                                               lz ++ variants { as ++ wsp;
                                                                 wsp ++ as } } };
                                    
    TBorn t a = let ts = t.s in
                let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
                { s = ts ++ "naceu" ++ as; };
              
    TFormed t a = let ts = t.s in
                    let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
                   let f = table { NSg => variants {"se formou"; "se fundou"; "se creou" };
                                   NPl => variants {"se formaron"; "se fundaron"; "se crearon" } }
                            ! a.n in
                    { s = ts ++ f ++ as; };
              
    TDie t a = let ts = t.s in
               let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
               { s = ts ++ "morreu" ++ as; };
              
    TDissolved t a = let ts = t.s in
                      let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
                     let d = table { NSg => variants { "se separou"; "se disolviu" };
                                      NPl => variants { "se separaron"; "se disolvieron" } } ! a.n
                    in { s = ts ++ d ++ as; };
          
    TJoin t a z = let ts = t.s in
                  let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
                  let za = variants { z.s ! <PAa,DOo> ; z.s ! <PAa,DNon> } in
                  let ze = variants { z.s ! <PEn,DOo> ; z.s ! <PEn,DNon> } in
                   { s = ts ++ variants { "se uniu" ++ variants { as ++ za ;
                                                                 za ++ as };
                                         "entrou" ++ variants { as ++ ze ;
                                                                ze ++ as } } };
              
    TLeave t a z = let ts = t.s in
                  let as = variants { a.s ! <PNon,DOo> ; a.s ! <PNon,DNon> } in
                  let zs = variants { z.s ! <PNon,DOo> ; z.s ! <PNon,DNon> ;
                                      z.s ! <PAa,DOo> ; z.s ! <PAa,DNon> } in
                  let zd = variants { z.s ! <PDe,DOo> ; z.s ! <PDe,DNon> } in
                   { s = ts ++ variants { "saiu" ++ variants { as ++ zd ;
                                                               zd ++ as };
                                         "se foi" ++ variants { as ++ zd ;
                                                                zd ++ as };
                                         "marchou" ++ variants { as ++ zd ;
                                                               zd ++ as };                    
                                         "deixou" ++ variants { as ++ zs ;
                                                              zs ++ as } } };
          
    TSWhen = ss "cando";
    TSYear = ss "en que ano" ;
    
    
    -- answers
    
    
    TARelease a w d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> }
                     in let lz = table { NSg => "lanzou" ; NPl => "lanzaron" } ! a.n
                     in let ws = variants { w.s ! <PNon,DPos> ; w.s ! <PNon,DOo> }
                      in { s = as ++ lz ++ ws ++ "en" ++ d.s };
                      
    TAReleaseX a w x d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> }
                        in let lz = table { NSg => "lanzou" ; NPl => "lanzaron" } ! a.n
                        in let ws = variants { w.s ! <PNon,DOo> ; w.s ! <PNon,DNon> ; w.s ! <PNon,DPos> }
                        in let xs = variants { x.s ! <PDe,DPos> ; x.s ! <PDe,DNon> ; x.s ! <PDe,DOo> }
                        in  { s = as ++ lz ++ ws ++ "(" ++ xs ++ ")" ++ "en" ++ d.s };
  
    TABorn a d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> }
                in { s = as ++ "naceu en" ++ d.s };
                
    TADie a d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> }
                in { s = as ++ "morreu en" ++ d.s };
  
    TAFormed a d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> } 
                  in let f = table { NSg => "formouse" ; NPl => "formáronse" } ! a.n
                  in { s = as ++ f ++ "en" ++ d.s };
                  
    TADissolved a d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> }
                      in let f = table { NSg => "disolveuse" ; NPl => "separáronse" } ! a.n
                      in { s = as ++ f ++ "en" ++ d.s };
  
    TAJoin a b d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> } 
                  in let bs = variants { b.s ! <PAa,DNon> ; b.s ! <PAa,DOo> } 
                  in { s = as ++ "uniuse" ++ bs ++ "en" ++ d.s };
                  
    TALeave a b d = let as = variants { a.s ! <PNon,DNon> ; a.s ! <PNon,DOo> } 
                    in let bs = variants { b.s ! <PAa,DNon> ; b.s ! <PAa,DOo> ;
                                           b.s ! <PNon,DNon> ; b.s ! <PNon,DOo> } 
                    in { s = as ++ "deixou" ++ bs ++ "en" ++ d.s };
}