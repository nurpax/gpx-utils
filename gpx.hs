{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime)
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

deg2rad x = 2 * pi * x / 360
rad2deg x = x * 180 / pi

segmentLength :: Trkpt -> Trkpt -> Double
segmentLength a b =
  let t = deg2rad $ longitude a - longitude b in
  let d = (sin lat1) * (sin lat2) + (cos lat1) * (cos lat2) * (cos t) in
  60.0 * 1.1515 * 1.609344 * (rad2deg $ acos d) where
    lat1 = deg2rad $ latitude a
    lat2 = deg2rad $ latitude b

trackLength :: Trkseg -> Double
trackLength (Trkseg segs) =
  sum (map (uncurry $ segmentLength) segments) where
    segments = zip segs (tail segs)

main :: IO ()
main =
  do
    trackSegs <- runX (readDocument [withValidate no] "test/activity1.gpx" >>> getTrkseg)
    let len = trackLength $ head trackSegs
    putStrLn (printf "Track length: %f" len)
