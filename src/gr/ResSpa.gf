resource ResSpa = {

  param Genero = GMasc | GFem ;
  param Numero = NSg | NPl ;
  param Preposicion = PAa | PEn | PDe | PNo ;
  param Determinante = DEl | DPos | DNo ;

  oper pf : Preposicion => Str =
    table {
      PAa => "a" ;
      PEn => "en" ;
      PDe => "de" ;
      PNo => ""
    } ;
  
  oper df : (Genero*Numero) => Determinante => Str =
    table {
      <g,n> => table {
        DEl => case <g,n> of {
            <GMasc,NSg> => "el" ;
            <GMasc,NPl> => "los" ;
            <GFem,NSg> => "la" ;
            <GFem,NPl> => "las" 
          } ;
        DPos => case n of {
            NSg => "su" ;
            NPl => "sus"
          } ;
        DNo => ""
      }
    } ;

  oper stdflex : Str -> (Genero*Numero) => ((Preposicion*Determinante) => Str) = \s ->
    table {
      <GMasc,NSg> => table {
          <PAa,DEl> => "al" ++ s ;
          <PDe,DEl> => "del" ++ s ;
          <p,d> => pf ! p ++ df ! <GMasc,NSg> ! d ++ s
        } ;
      <g,n> => table {
          <p,d> => pf ! p ++ df ! <g,n> ! d ++ s
        }
      } ;
    
  oper fzflex : Str -> (Genero*Numero) => ((Preposicion*Determinante) => Str) = \s ->
    table {
      <GMasc,NSg> => table {
        <PAa,DEl> => "al" ++ s ;
        <PAa,DNo> => "al" ++ s ;
        <PDe,DEl> => "del" ++ s ;
        <PDe,DNo> => "del" ++ s ;
        <p,DEl> => pf ! p ++ df ! <GMasc,NSg> ! DEl ++ s ;
        <p,DNo> => pf ! p ++ df ! <GMasc,NSg> ! DEl ++ s ;
        <p,d> => pf ! p ++ df ! <GMasc,NSg> ! d ++ s
      } ;
      <g,n> => table {
        <p,DEl> => pf ! p ++ df ! <g,n> ! DEl ++ s ;
        <p,DNo> => pf ! p ++ df ! <g,n> ! DEl ++ s ;
        <p,d> => pf ! p ++ df ! <g,n> ! d ++ s      
      }
    } ;
}