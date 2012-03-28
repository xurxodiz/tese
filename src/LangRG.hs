module LangRG where
  
import Grammar
import Types

g_noparse :: GAnswer
g_noparse = GAENoParse

g_notimplemented :: GAnswer
g_notimplemented = GAENotImplemented

g_nosense :: GAnswer
g_nosense = GAENoSense

g_authormissing :: GAnswer
g_authormissing = GAEAuthorMissing

g_noresults :: GAnswer
g_noresults = GAENoResults

g_namedauthor :: Artist -> GAuthor
g_namedauthor a = GANamed (GTStr (GString a))

g_namedband :: Artist -> GAuthor
g_namedband a = GANamedUnit GAUTBand (GAUNName (GTStr (GString a)))

g_namedsinger :: Artist -> GAuthor
g_namedsinger a = GANamedUnit GAUTSinger (GAUNName (GTStr (GString a)))

g_namedsingle :: Album -> GWork
g_namedsingle w = GWNamedUnit GWUTSingle (GWUNName (GTStr (GString w)))

g_namedsinglefirst :: Album -> GWork
g_namedsinglefirst w = GWNamedModUnit GWUTSingle GWUMFirst (GWUNName (GTStr (GString w)))

g_namedsinglelast :: Album -> GWork
g_namedsinglelast w = GWNamedModUnit GWUTSingle GWUMLast (GWUNName (GTStr (GString w)))

g_namedalbum :: Album -> GWork
g_namedalbum w = GWNamedUnit GWUTAlbum (GWUNName (GTStr (GString w)))

g_namedalbumfirst :: Album -> GWork
g_namedalbumfirst w = GWNamedModUnit GWUTAlbum GWUMFirst (GWUNName (GTStr (GString w)))

g_namedalbumlast :: Album -> GWork
g_namedalbumlast w = GWNamedModUnit GWUTAlbum GWUMLast (GWUNName (GTStr (GString w)))

g_namedsong :: Track -> GWork
g_namedsong w = GWNamedUnit GWUTSong (GWUNName (GTStr (GString w)))

g_nameddate :: Date -> GDate
g_nameddate d = GDNamed (GTStr (GString d))

g_listdate :: [Date] -> GListDate
g_listdate l = GListDate [g_nameddate x | x <- l]



