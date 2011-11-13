{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime, diffUTCTime)
import Locale (defaultTimeLocale)
import Text.Printf (printf)

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
text = getChildren >>> getText

getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- text <<< atTag "time" -< x
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

-- Haversine's formula for computing the length of a segment expressed
-- in longitude/latitude.
--
-- From http://www.movable-type.co.uk/scripts/latlong.html
segmentLength :: Trkpt -> Trkpt -> Double
segmentLength pta ptb = r * c where
  r = 6371 -- Earth's mean radius
  deg2rad x = 2 * pi * x / 360
  lat1 = deg2rad $ latitude pta
  lat2 = deg2rad $ latitude ptb
  dlat = lat2 - lat1
  dlon = deg2rad $ longitude ptb - longitude pta
  a = (sin dlat/2)^2 + (sin dlon/2)^2 * cos lat1 * cos lat2
  c = 2 * atan2 (sqrt a) (sqrt (1-a))

-- Length of track as (seconds, kms)
trackLength :: Trkseg -> (Double, Double)
trackLength (Trkseg segs) = (timeLen, kmLen) where
    kmLen = sum (map (uncurry segmentLength) segments)
    timeLen = sum (map (uncurry timeDelta) segments)
    segments = zip segs (tail segs)
    timeDelta :: Trkpt -> Trkpt -> Double
    timeDelta a b = realToFrac $ diffUTCTime (time b) (time a)

formatTimeDeltaHMS :: Double -> String
formatTimeDeltaHMS s =
  show (floor $ s / 60 / 60) ++ ":" ++
  show (floor (s / 60) `mod` 60) ++ ":" ++
  show (floor s `mod` 60)

formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s =
  show (floor $ s / 60) ++ ":" ++ show (floor s `mod` 60)

main :: IO ()
main =
  do
    trackSegs <- runX (readDocument [withValidate no] "test/activity1.gpx" >>> getTrkseg)
    let (seconds, lenKm) = trackLength $ head trackSegs
    putStrLn (printf "Track distance (km):     %f" lenKm)
    putStrLn (printf "Track duration (h:m:s):  %s" $ formatTimeDeltaHMS seconds)
    putStrLn (printf "Average pace (min/km):   %s" $ formatTimeDeltaMS (seconds / lenKm))
