module MBFetch where
  
import Codec.Binary.Url as U
import Codec.Binary.UTF8.String as SU
import Control.Concurrent
import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import XMLUtils
import Types

{- for a utf-8 encoded name in a string:
   # first decodes it to haskell-string format
   # then encodes it back to utf-8, but in word8 representation
  # finally encodes those word8 back into an URL-apt string
-}
encodeForUrl :: (Arrow a) => a String String
encodeForUrl = arr $ U.encode . SU.encode . SU.decodeString . replaceSpaces
  where replaceSpaces [] = []
        replaceSpaces ('_':t) = ' ':(replaceSpaces t)
        replaceSpaces (h:t) = h:(replaceSpaces t)

-- waits the imposed delay by MusicBrainz between queries, one second
delay :: IOStateArrow s String String
delay = arr (\x -> let _ = threadDelay 1000000 in x)

-- reads a xml from a given remote url skipping dtd validation
readRemoteXml :: IOStateArrow s String XmlTree
readRemoteXml = readDocument [withValidate no,
                              withCurl [("curl--user-agent", "dizTese/1.0")]]
                $< this

-- waits as forced upon us by MB, then reads the document
-- NOTE: don't use `guards`!
--     delay may not happen (due to concurrency?)
fetchXml :: IOStateArrow s String XmlTree
fetchXml = delay >>> readRemoteXml

fetchSingleXmlByTitle :: IOStateArrow s Album XmlTree
fetchSingleXmlByTitle = encodeForUrl
                       >>>
                       arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Single&title="++)
                       >>> fetchXml
                      
fetchSingleXmlByArtist :: IOStateArrow s Artist XmlTree
fetchSingleXmlByArtist = encodeForUrl
                       >>>
                       arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Single+Official&limit=100&artist="++)
                       >>> fetchXml
                      
fetchSingleXmlByArtistId :: IOStateArrow s MBID XmlTree
fetchSingleXmlByArtistId = arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Single+Official&limit=100&artistid="++)
                             >>> fetchXml
                      
fetchSingleXmlByTitleArtist :: IOStateArrow s (Album, Artist) XmlTree
fetchSingleXmlByTitleArtist = (encodeForUrl *** encodeForUrl)
                       >>>
                       arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Single&title="++x++"&artist="++y
                        )
                       >>> fetchXml
                      
fetchSingleXmlByTitleArtistId :: IOStateArrow s (Album, MBID) XmlTree
fetchSingleXmlByTitleArtistId = first encodeForUrl
                       >>>
                       arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Single&title="++x++"&artistid="++y
                        )
                       >>> fetchXml
                      
fetchAlbumXmlByTitle :: IOStateArrow s Album XmlTree
fetchAlbumXmlByTitle = encodeForUrl
                      >>>
                      arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Album&title="++)
                      >>> fetchXml
                      
fetchAlbumXmlByArtist :: IOStateArrow s Artist XmlTree
fetchAlbumXmlByArtist = encodeForUrl
                       >>>
                       arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Album+Official&limit=100&artist="++)
                       >>> fetchXml
                      
fetchAlbumXmlByArtistId :: IOStateArrow s Artist XmlTree
fetchAlbumXmlByArtistId = arr ("http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Album+Official&limit=100&artistid="++)
                       >>> fetchXml
                      
fetchAlbumXmlByTitleArtist :: IOStateArrow s (Album, Artist) XmlTree
fetchAlbumXmlByTitleArtist = (encodeForUrl *** encodeForUrl)
                      >>>
                       arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Album&title="++x++"&artist="++y
                        )
                      >>> fetchXml
                      
fetchAlbumXmlByTitleArtistId :: IOStateArrow s (Album, MBID) XmlTree
fetchAlbumXmlByTitleArtistId = first encodeForUrl
                      >>>
                       arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/release/?type=xml&releasetypes=Album&title="++x++"&artistid="++y
                        )
                      >>> fetchXml
                      
fetchTrackXmlByTitle :: IOStateArrow s Track XmlTree
fetchTrackXmlByTitle = encodeForUrl
                      >>>
                      arr ("http://musicbrainz.org/ws/1/track/?type=xml&title="++)
                      >>> fetchXml
                      
fetchTrackXmlByTitleArtist :: IOStateArrow s (Track, Artist) XmlTree
fetchTrackXmlByTitleArtist = (encodeForUrl *** encodeForUrl)
                      >>>
                      arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/track/?type=xml&title="++x++"&artist="++y
                        )
                      >>> fetchXml
                      
fetchTrackXmlByTitleArtistId :: IOStateArrow s (Track, Artist) XmlTree
fetchTrackXmlByTitleArtistId = first encodeForUrl
                      >>>
                      arr (\(x,y) ->
                        "http://musicbrainz.org/ws/1/track/?type=xml&title="++x++"&artistid="++y
                        )
                      >>> fetchXml
                      
fetchReleaseEventsXmlById :: IOStateArrow s MBID XmlTree
fetchReleaseEventsXmlById = arr (\x -> "http://musicbrainz.org/ws/1/release/"++x++"/?type=xml&inc=release-events")
                            >>> fetchXml
                            
fetchArtistXmlByName :: IOStateArrow s Artist XmlTree
fetchArtistXmlByName = arr ("http://musicbrainz.org/ws/1/artist/?type=xml&name="++)
                        >>> fetchXml
                        
fetchArtistRelsXmlById :: IOStateArrow s MBID XmlTree
fetchArtistRelsXmlById = arr (\x -> "http://musicbrainz.org/ws/1/artist/"++x++"?type=xml&inc=artist-rels")
                          >>> fetchXml

fetchSingleByTitle :: IOStateArrow s Album XmlTree
fetchSingleByTitle = fetchSingleXmlByTitle >>> atTag "release"

fetchSingleByArtist :: IOStateArrow s Artist XmlTree
fetchSingleByArtist = fetchSingleXmlByArtist >>> atTag "release"

fetchSingleByArtistId :: IOStateArrow s MBID XmlTree
fetchSingleByArtistId = fetchSingleXmlByArtistId >>> atTag "release"

fetchSingleByTitleArtist :: IOStateArrow s (Album, Artist) XmlTree
fetchSingleByTitleArtist = fetchSingleXmlByTitleArtist >>> atTag "release"

fetchSingleByTitleArtistId :: IOStateArrow s (Album, MBID) XmlTree
fetchSingleByTitleArtistId = fetchSingleXmlByTitleArtistId >>> atTag "release"
                                  
fetchAlbumByTitle :: IOStateArrow s Album XmlTree
fetchAlbumByTitle = fetchAlbumXmlByTitle >>> atTag "release"

fetchAlbumByArtist :: IOStateArrow s Artist XmlTree
fetchAlbumByArtist = fetchAlbumXmlByArtist >>> atTag "release"

fetchAlbumByArtistId :: IOStateArrow s Artist XmlTree
fetchAlbumByArtistId = fetchAlbumXmlByArtistId >>> atTag "release"

fetchAlbumByTitleArtist :: IOStateArrow s (Album, Artist) XmlTree
fetchAlbumByTitleArtist = fetchAlbumXmlByTitleArtist >>> atTag "release"

fetchAlbumByTitleArtistId :: IOStateArrow s (Album, MBID) XmlTree
fetchAlbumByTitleArtistId = fetchAlbumXmlByTitleArtistId >>> atTag "release"

fetchTrackByTitle :: IOStateArrow s Track XmlTree
fetchTrackByTitle = fetchTrackXmlByTitle >>> atTag "track"

fetchTrackByTitleArtist :: IOStateArrow s (Track, Artist) XmlTree
fetchTrackByTitleArtist = fetchTrackXmlByTitleArtist >>> atTag "track"

fetchTrackByTitleArtistId :: IOStateArrow s (Track, MBID) XmlTree
fetchTrackByTitleArtistId = fetchTrackXmlByTitleArtistId >>> atTag "track"
                
fetchReleaseEventsById :: IOStateArrow s MBID XmlTree
fetchReleaseEventsById = fetchReleaseEventsXmlById >>> atTag "release"

fetchArtistByName :: IOStateArrow s Artist XmlTree
fetchArtistByName = fetchArtistXmlByName >>> atTag "artist"

fetchArtistRelsById :: IOStateArrow s MBID XmlTree
fetchArtistRelsById = fetchArtistRelsXmlById >>> atTag "artist"