resource ResGal = {

  param Xenero = XMasc | XFem ;
  param Numero = NSg | NPl ;
  param Preposicion = PAa | PEn | PDe | PNon ;
  param Determinante = DOo | DPos | DNon ;

  oper pf : Preposicion => Str =
    table {
      PAa => "a" ;
      PEn => "en" ;
      PDe => "de" ;
      PNon => ""
    } ;
  
  oper df : (Xenero*Numero) => Determinante => Str =
    table {
      <x,n> => table {
        DOo => case <x,n> of {
            <XMasc,NSg> => "o" ;
            <XMasc,NPl> => "os" ;
            <XFem,NSg> => "a" ;
            <XFem,NPl> => "as" 
          } ;
        DPos => case <x,n> of {
            <XMasc,NSg> => "o seu" ;
            <XMasc,NPl> => "os seus" ;
            <XFem,NSg> => "a súa" ;
            <XFem,NPl> => "as súas" 
          } ;
        DNon => ""
      }
    } ;

  oper stdflex : Str -> (Xenero*Numero) => ((Preposicion*Determinante) => Str) = \s ->
    table {
      <XMasc,NSg> => table {
          <PAa,DOo> => variants{"ao"; "ó"} ++ s ;
          <PAa,DPos> => variants{"ao"; "ó"} ++ "seu" ++ s ;
          <PEn,DOo> => "no" ++ s ;
          <PEn,DPos> => "no seu" ++ s ;
          <PDe,DOo> => "do" ++ s ;
          <PDe,DPos> => "do seu" ++ s;
          <PNon,d> => df ! <XMasc,NSg> ! d ++ s ;
          <p,DNon> => pf ! p ++ s
        } ;
      <XMasc,NPl> => table {
          <PAa,DOo> => variants{"aos"; "ós"} ++ s ;
          <PAa,DPos> => variants{"aos"; "ós"} ++ "seus" ++ s ;
          <PEn,DOo> => "nos" ++ s ;
          <PEn,DPos> => "nos seus" ++ s ;
          <PDe,DOo> => "dos" ++ s ;
          <PDe,DPos> => "dos seus" ++ s;
          <PNon,d> => df ! <XMasc,NPl> ! d ++ s ;
          <p,DNon> => pf ! p ++ s
        } ;
      <XFem,NSg> => table {
          <PAa,DOo> => "á" ++ s ;
          <PAa,DPos> => "á súa" ++ s ;
          <PEn,DOo> => "na" ++ s ;
          <PEn,DPos> => "na súa" ++ s ;
          <PDe,DOo> => "da" ++ s ;
          <PDe,DPos> => "da súa" ++ s;
          <PNon,d> => df ! <XFem,NSg> ! d ++ s ;
          <p,DNon> => pf ! p ++ s
        } ;
      <XFem,NPl> => table {
          <PAa,DOo> => "ás" ++ s ;
          <PAa,DPos> => "ás súas" ++ s ;
          <PEn,DOo> => "nas" ++ s ;
          <PEn,DPos> => "nas súas" ++ s ;
          <PDe,DOo> => "das" ++ s ;
          <PDe,DPos> => "das súas" ++ s;
          <PNon,d> => df ! <XFem,NPl> ! d ++ s ;
          <p,DNon> => pf ! p ++ s
        }
      } ;
    
  oper fzflex : Str -> (Xenero*Numero) => ((Preposicion*Determinante) => Str) = \s ->
    table {
      <x,n> => table {
        <p,DNon> => (stdflex s) ! <x,n> ! <p,DOo> ;
        <p,d>    => (stdflex s) ! <x,n> ! <p,d>
      }
    } ;
}