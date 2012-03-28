module XMLUtils where
  
import Control.Arrow
import Text.XML.HXT.Core
      
-- all tags with a given name
atTag :: (ArrowXml a) => String -> a XmlTree XmlTree  
atTag tag = deep (isElem >>> hasName tag)

filterTag :: (ArrowXml a) => String -> a XmlTree XmlTree
filterTag s = (atTag s >>> filterByScore 100) >>. take 3

filterByScore :: (ArrowXml a) => Int ->  a XmlTree XmlTree
filterByScore i = hasAttrValue "ext:score" ((>= i) . read)

filterArtist :: (ArrowXml a) => a XmlTree XmlTree
filterArtist = filterTag "artist"

filterTrack :: (ArrowXml a) => a XmlTree XmlTree
filterTrack = filterTag "track"

getMyTitle :: (ArrowXml a) => a XmlTree String
getMyTitle = getChildren >>> hasName "title" >>> getChildren >>> getText

getMyName :: (ArrowXml a) => a XmlTree String
getMyName = getChildren >>> hasName "name" >>> getChildren >>> getText

getMyId :: (ArrowXml a) => a XmlTree String
getMyId = hasAttrValue "id" (not . null) >>> getAttrValue "id"

getMyBegin :: (ArrowXml a) => a XmlTree String
getMyBegin = hasAttrValue "begin" (not . null) >>> getAttrValue "begin"

getMyEnd :: (ArrowXml a) => a XmlTree String
getMyEnd = hasAttrValue "end" (not . null) >>> getAttrValue "end"

getMyDate :: (ArrowXml a) => a XmlTree String
getMyDate = hasAttrValue "date" (not . null) >>> getAttrValue "date"

getReleaseId :: (ArrowXml a) => a XmlTree String
getReleaseId = atTag "release" >>> getMyId

getArtistName :: (ArrowXml a) => a XmlTree String
getArtistName = atTag "artist" >>> getMyName

getArtistId :: (ArrowXml a) => a XmlTree String
getArtistId = atTag "artist" >>> getMyId

getReleaseTitle :: (ArrowXml a) => a XmlTree String
getReleaseTitle = atTag "release" >>> getMyName

getEventDate :: (ArrowXml a) => a XmlTree String
getEventDate = atTag "event" >>> getMyDate

getLifeBegin :: (ArrowXml a) => a XmlTree String
getLifeBegin = atTag "life-span" >>> getMyBegin

getLifeEnd :: (ArrowXml a) => a XmlTree String
getLifeEnd = atTag "life-span" >>> getMyEnd

getMembersRelation :: (ArrowXml a) => a XmlTree XmlTree
getMembersRelation = atTag "relation" >>> isTypeMember

getRelationBegin :: (ArrowXml a) => a XmlTree String
getRelationBegin = atTag "relation" >>> getMyBegin

getRelationEnd :: (ArrowXml a) => a XmlTree String
getRelationEnd = atTag "relation" >>> getMyEnd

isTypeMember :: (ArrowXml a) => a XmlTree XmlTree
isTypeMember = hasAttrValue "type" ((==) "MemberOfBand")

isTypePerson :: (ArrowXml a) => a XmlTree XmlTree
isTypePerson = hasAttrValue "type" ((==) "Person")

isTypeGroup :: (ArrowXml a) => a XmlTree XmlTree
isTypeGroup = hasAttrValue "type" ((==) "Group")

isDirectionBackward :: (ArrowXml a) => a XmlTree XmlTree
isDirectionBackward = hasAttrValue "direction" ((==) "backward")