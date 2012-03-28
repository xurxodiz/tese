resource ResEng = {

  param Determiner = DArt | DPos | DNone ;
  param Article = AThe | ANone ;
  param Number = NSg | NPl | NUk;

  oper detflex : Str -> (Determiner => (Number => Str)) = \x ->
    table {
      DArt => table { _ => (artflex x) ! AThe } ;
      DPos => table {
        NSg => variants { "his" ; "her" ; "its" } ++ x ;
        NPl => "their" ++ x;
        NUk => variants { "the"; "its"; "their"; "his"; "her" } ++ x
        -- "the" is used as default because of the number/gender problem
        -- but all are accepted, the user knows best!
      } ;
      DNone => table { _ => (artflex x) ! ANone }
    } ;

  oper artflex : Str -> (Article => Str) = \x ->
    table {
      AThe => "the" ++ x;
      ANone => x
    } ;
  
}