module Main where

import Prelude

import Chart (renderDays)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filter, groupBy, mapMaybe, nub, range, (:))
import Data.Date (Date, Month, Weekday(..), canonicalDate, weekday)
import Data.DateTime (DateTime(DateTime), adjust, date)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (sum)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Int (toNumber)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, un)
import Data.NonEmpty ((:|))
import Data.Time.Duration (Days(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Halogen.HTML (HTML(..))
import Halogen.VDom.DOM.StringRenderer (render)
import Network.HTTP.Affjax (AJAX)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (writeTextFile)
import Node.Process (PROCESS, argv)
import Partial.Unsafe (unsafePartial)
import Strava.API.Activities (ActivityRangeStart(..), AuthToken(..), getMany)
import Strava.Activity (Activity, ActivityType(..), Metres)

date' :: Int -> Month -> Int -> Maybe Date
date' year month day =
  canonicalDate <$> toEnum year <*> pure month <*> toEnum day

week :: Date -> Array Date
week d = map date $ mapMaybe (flip adjust $ DateTime d bottom) $ Days <$> toNumber <$> range (-6) 0

actDate :: Activity -> Date
actDate = date <<< unwrap <<< _.startDate <<< unwrap

toSunday :: Date -> Maybe Date
toSunday d = date <$> adjust (Days diff) (DateTime d bottom)
  where diff = toNumber $ fromEnum Sunday - fromEnum (weekday d)

generateCharts :: forall eff. AuthToken -> String -> Aff (console :: CONSOLE, fs :: FS, ajax :: AJAX | eff) Unit
generateCharts token outputDir = do
  acts <- getMany token { start: Default, pageCount: 200 }
  let runs = filter ((==) Run <<< _.type' <<< unwrap) acts
      sundays = nub $ mapMaybe toSunday $ (date <<< unwrap <<< _.startDate <<< unwrap) <$> runs
      byDate = groupBy (\a b -> actDate a == actDate b) runs
      totalMap :: Map Date Metres
      totalMap = fromFoldable $ (\(r:|runs) -> Tuple (actDate r) (sum $ (_.distance <<< unwrap) <$> r:runs )) <$> byDate
      forWeek :: Date -> Array Number
      forWeek d = (maybe 0.0 unwrap) <$> flip lookup totalMap <$> week d
  for_ sundays $ \d -> do
    let componentHtml = (renderDays $ forWeek d)
        html = render (const "") (un HTML componentHtml) 
        dateStr = format <$> (parseFormatString "YYYY-MM-DD") <*> pure (DateTime d bottom)
    case dateStr of
      Left _ -> pure unit
      Right s -> do
        liftEff $ log $ s <> ": " <> show (forWeek d)
        let filename = outputDir <> "/" <> s <> ".html"
        liftEff $ log $ "Wrote file: " <> filename
        writeTextFile UTF8 filename html

main :: forall a. Eff (exception :: EXCEPTION, console :: CONSOLE, fs :: FS, ajax :: AJAX, process :: PROCESS | a) Unit
main = void $ launchAff $ unsafePartial $ do
  args <- liftEff argv
  case args of
    [ _, _, token, outDir ] -> generateCharts (AuthToken token) outDir
    _ -> liftEff $ log $ "Usage: weekchart <token> <outputDir>"
