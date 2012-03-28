module MB where
  
import Control.Monad
import Control.Arrow
import Text.XML.HXT.Core

import Debug.Trace

import MBFetch
import MBSieve
import Types
import XMLUtils
          
-- this line defines the entry point for the main module  
pipe :: b -> IOSArrow b c -> (c -> String) -> IO [String]
pipe input arw reply = let raw = runX $ constA input >>> arw
                        in liftM (map reply) raw

-- these functions are the link to the main module

a_treleaseNamed :: IOSArrow Token (Track, Artist, Date, Album) -- track may be empty
a_treleaseNamed = (a_treleaseSong
                     <+>
                   (a_treleaseAlbum >>. s_expand34)) >>. s_treleaseNamed

a_treleaseSingle :: IOSArrow Album (Album, Artist, Date)
a_treleaseSingle = (fetchSingleByTitle >>> (getMyTitle &&& getArtistName) &&& getEventDate)
                    >>. s_treleaseSingle

a_treleaseAlbum :: IOSArrow Album (Album, Artist, Date)
a_treleaseAlbum = (fetchAlbumByTitle >>> (getMyTitle &&& getArtistName) &&& getEventDate)
                  >>. s_treleaseAlbum

a_treleaseSong :: IOSArrow Track (Track, Artist, Date, Album)
a_treleaseSong = (fetchTrackByTitle >>> filterTrack >>> ((getMyTitle &&& getArtistName)
                                        &&&
                                        (getReleaseId >>> fetchReleaseEventsById >>> (getEventDate
                                                                                      &&&  getMyTitle))))
                  >>. s_treleaseSong
                  
                  
a_treleasebyNamed :: IOSArrow (Token, Artist) (Track, Artist, Date, Album) -- track may be empty
a_treleasebyNamed = (a_treleasebySong
                     <+>
                     (a_treleasebyAlbum >>. s_expand34)) >>. s_treleasebyNamed

a_treleasebySingle :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebySingle = (fetchSingleByTitleArtist >>> (getMyTitle &&& getArtistName) &&& getEventDate)
                    >>. s_treleasebySingle

a_treleasebyAlbum :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebyAlbum = (fetchAlbumByTitleArtist >>> (getMyTitle &&& getArtistName) &&& getEventDate)
                  >>. s_treleasebyAlbum

a_treleasebySong :: IOSArrow (Track, Artist) (Track, Artist, Date, Album)
a_treleasebySong = (fetchTrackByTitleArtist >>> filterTrack >>> ((getMyTitle &&& getArtistName)
                                        &&&
                                        (getReleaseId >>> fetchReleaseEventsById >>> (getEventDate
                                                                                      &&&  getMyTitle))))
                  >>. s_treleasebySong
                  
a_treleasebyFirstSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyFirstSingle = (fetchSingleByArtist >>> (getArtistName &&& (getMyTitle &&& getEventDate)))
                          >>. s_treleasebyFirstSingle
                          
a_treleasebyFirstAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyFirstAlbum = (fetchAlbumByArtist >>> (getArtistName &&& (getMyTitle &&& getEventDate)))
                          >>. s_treleasebyFirstAlbum
                          
a_treleasebyFirstSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyFirstSong = a_treleasebyFirstSingle -- semantics, after all
                          
a_treleasebyLastSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyLastSingle = (fetchSingleByArtist >>> ((getArtistName &&& getMyTitle) &&& getEventDate))
                          >>. s_treleasebyLastSingle

a_treleasebyLastAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyLastAlbum = (fetchAlbumByArtist >>> ((getArtistName &&& getMyTitle) &&& getEventDate))
                          >>. s_treleasebyLastAlbum
                          
a_treleasebyLastSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyLastSong = a_treleasebyLastSingle -- semantics, after all

a_treleasebyBandNamed :: IOSArrow (Token, Artist) (Track, Artist, Date, Album) -- track may be empty
a_treleasebyBandNamed = (a_treleasebyBandSong
                         <+>
                         (a_treleasebyBandAlbum >>. s_expand34)) >>. s_treleasebyNamed

a_treleasebyBandSingle :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebyBandSingle = (second (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId)
                           >>> fetchSingleByTitleArtistId >>>
                            ((getMyTitle &&& getArtistName) &&& getEventDate)
                         ) >>. s_treleasebySingle

a_treleasebyBandAlbum :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebyBandAlbum = (second (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId)
                           >>> fetchAlbumByTitleArtistId >>>
                            ((getMyTitle &&& getArtistName) &&& getEventDate)
                         ) >>. s_treleasebyAlbum

a_treleasebyBandSong :: IOSArrow (Track, Artist) (Track, Artist, Date, Album)
a_treleasebyBandSong = (second (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId)
                           >>> (fetchTrackByTitleArtistId >>> filterTrack >>> ((getMyTitle &&& getArtistName)
                                        &&&
                                        (getReleaseId >>> fetchReleaseEventsById >>> (getEventDate
                                                                                      &&&  getMyTitle))))
                       ) >>. s_treleasebySong
                  
a_treleasebyBandFirstSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandFirstSingle = (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId
                                >>> fetchSingleByArtistId >>>
                                (getArtistName &&& (getMyTitle &&& getEventDate))
                               ) >>. s_treleasebyFirstSingle
                          
a_treleasebyBandFirstAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandFirstAlbum = (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId
                               >>> fetchAlbumByArtistId >>>
                                (getArtistName &&& (getMyTitle &&& getEventDate))
                               ) >>. s_treleasebyFirstAlbum
                          
a_treleasebyBandFirstSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandFirstSong = a_treleasebyBandFirstSingle -- semantics, after all
                          
a_treleasebyBandLastSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandLastSingle = (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId
                                >>> fetchSingleByArtistId >>>
                                ((getArtistName &&& getMyTitle) &&& getEventDate)
                               ) >>. s_treleasebyLastSingle

a_treleasebyBandLastAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandLastAlbum = (fetchArtistByName >>> isTypeGroup >>> filterArtist >>> getMyId
                               >>> fetchAlbumByArtistId >>>
                                ((getArtistName &&& getMyTitle) &&& getEventDate)
                               ) >>. s_treleasebyLastAlbum
                          
a_treleasebyBandLastSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebyBandLastSong = a_treleasebyBandLastSingle -- semantics, after all

a_treleasebySingerNamed :: IOSArrow (Token, Artist) (Track, Artist, Date, Album) -- track may be empty
a_treleasebySingerNamed = (a_treleasebySingerSong
                           <+>
                           (a_treleasebySingerAlbum >>. s_expand34)) >>. s_treleasebyNamed

a_treleasebySingerSingle :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebySingerSingle = (second (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId)
                             >>> fetchSingleByTitleArtistId >>>
                              ((getMyTitle &&& getArtistName) &&& getEventDate)
                            ) >>. s_treleasebySingle

a_treleasebySingerAlbum :: IOSArrow (Album, Artist) (Album, Artist, Date)
a_treleasebySingerAlbum = (second (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId)
                             >>> fetchAlbumByTitleArtistId >>>
                            ((getMyTitle &&& getArtistName) &&& getEventDate)
                          ) >>. s_treleasebyAlbum

a_treleasebySingerSong :: IOSArrow (Track, Artist) (Track, Artist, Date, Album)
a_treleasebySingerSong = (second (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId)
                           >>> (fetchTrackByTitleArtistId >>> filterTrack >>> ((getMyTitle &&& getArtistName)
                                        &&&
                                        (getReleaseId >>> fetchReleaseEventsById >>> (getEventDate
                                                                                      &&&  getMyTitle))))
                          ) >>. s_treleasebySong
                  
a_treleasebySingerFirstSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerFirstSingle = (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId
                                   >>> fetchSingleByArtistId >>>
                                 (getArtistName &&& (getMyTitle &&& getEventDate))
                                 ) >>. s_treleasebyFirstSingle
                          
a_treleasebySingerFirstAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerFirstAlbum = (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId
                                 >>> fetchAlbumByArtistId >>>
                                (getArtistName &&& (getMyTitle &&& getEventDate))
                                ) >>. s_treleasebyFirstAlbum
                          
a_treleasebySingerFirstSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerFirstSong = a_treleasebySingerFirstSingle -- semantics, after all
                          
a_treleasebySingerLastSingle :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerLastSingle = (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId
                                 >>> fetchSingleByArtistId >>>
                                ((getArtistName &&& getMyTitle) &&& getEventDate)
                                ) >>. s_treleasebyLastSingle

a_treleasebySingerLastAlbum :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerLastAlbum = (fetchArtistByName >>> isTypePerson >>> filterArtist >>> getMyId
                                >>> fetchAlbumByArtistId >>>
                                ((getArtistName &&& getMyTitle) &&& getEventDate)
                               ) >>. s_treleasebyLastAlbum
                          
a_treleasebySingerLastSong :: IOSArrow Artist (Artist, Album, Date)
a_treleasebySingerLastSong = a_treleasebySingerLastSingle -- semantics, after all

a_tbornNamed :: IOSArrow Artist (Artist, Date)
a_tbornNamed = (fetchArtistByName >>> isTypePerson >>> (getArtistName &&& getLifeBegin)) >>. s_tbornNamed

a_tbornNamedSinger :: IOSArrow Artist (Artist, Date)
a_tbornNamedSinger = a_tbornNamed

a_tdieNamed :: IOSArrow Artist (Artist, Date)
a_tdieNamed = (fetchArtistByName >>> isTypePerson >>> (getArtistName &&& getLifeEnd)) >>. s_tdieNamed

a_tdieNamedSinger :: IOSArrow Artist (Artist, Date)
a_tdieNamedSinger = a_tdieNamed

a_tformedNamed :: IOSArrow Artist (Artist, Date)
a_tformedNamed = (fetchArtistByName >>> isTypeGroup >>> (getArtistName &&& getLifeBegin)) >>. s_tformedNamed

a_tformedNamedBand :: IOSArrow Artist (Artist, Date)
a_tformedNamedBand = a_tformedNamed

a_tdissolvedNamed :: IOSArrow Artist (Artist, Date)
a_tdissolvedNamed = (fetchArtistByName >>> isTypeGroup >>> (getArtistName &&& getLifeEnd)) >>. s_tdissolvedNamed

a_tdissolvedNamedBand :: IOSArrow Artist (Artist, Date)
a_tdissolvedNamedBand = a_tdissolvedNamed

a_tjoin :: IOSArrow (Artist, Artist) (Artist, Artist, [Date])
a_tjoin = ((fetchArtistByName >>> filterArtist >>> getArtistId)
          *** (fetchArtistByName >>> filterArtist >>> getArtistId >>>
                      fetchArtistRelsById >>>  (getArtistName
                                &&& (getMembersRelation >>> isDirectionBackward >>>
                                      (getArtistId &&& (getArtistName &&& getRelationBegin))
                                    )
                      )
            )
          ) >>. s_tjoin
          
a_tleave :: IOSArrow (Artist, Artist) (Artist, Artist, [Date])
a_tleave = ((fetchArtistByName >>> filterArtist >>> getArtistId)
          *** (fetchArtistByName >>> filterArtist >>> getArtistId >>>
                      fetchArtistRelsById >>>  (getArtistName
                                &&& (getMembersRelation >>> isDirectionBackward >>>
                                      (getArtistId &&& (getArtistName &&& getRelationEnd))
                                    )
                      )
            )
          ) >>. s_tleave