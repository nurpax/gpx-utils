{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime)
import Locale (defaultTimeLocale)

data Trkseg = Trkseg [Trkpt] deriving (Eq, Show)

data Trkpt = Trkpt {
  time :: UTCTime,
  longitude :: Double,
  latitude :: Double
  } deriving (Eq, Show)

-- Note: the hardcoded .000 part is kludge but for my inputs this was
-- an easy way to get timestamps to parse.
readt :: String -> UTCTime
readt = readTime defaultTimeLocale "%FT%T.000%Z"

atTag tag = deep (isElem >>> hasName tag)

getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- getText <<< atTag "time" -< x
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    returnA -< Trkpt {
      time = readt time_,
      longitude = read lon,
      latitude = read lat
      }

getTrkseg = atTag "trkseg" >>>
  proc x -> do
    segments <- listA getTrkpt -< x
    returnA -< Trkseg segments


main :: IO ()
main =
--  runX (readDocument [withValidate no] "test/activity1_short.gpx" >>> getTrkseg)
  putStrLn "foo"
