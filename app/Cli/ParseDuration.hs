module Cli.ParseDuration where


import Options

import Data.Attoparsec.Text
import Data.Text

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

parseDuration :: String -> Either String ParseDuration
parseDuration str = parseOnly (parseDuration' <* endOfInput) (pack str)
  where
    parseDuration' :: Parser ParseDuration
    parseDuration' = addDuration <$> parseCalendarDiffTime

    addDuration :: CalendarDiffTime -> ZonedTime -> UTCTime
    addDuration (CalendarDiffTime months difftime) (ZonedTime (LocalTime day time) tz)
      = localTimeToUTC tz $ difftime `addLocalTime` (LocalTime (months `addGregorianMonthsRollOver` day) time)

parseCalendarDiffTime :: Parser CalendarDiffTime
parseCalendarDiffTime
  = mkDiffTime
  <$> duration "year" <*> duration "month" <*> duration "week" <*> duration "day" <*> duration "hour" <*> duration "minute"

mkDiffTime :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> CalendarDiffTime
mkDiffTime years months weeks days hours minutes
  =  (calendarTimeDays $ (-years) |* calendarYear <> (-months) |* calendarMonth <> (-weeks) |* calendarWeek <> (-days) |* calendarDay)
  <> (calendarTimeTime $ secondsToNominalDiffTime $ fromInteger $ negate 60 * (60 * hours + minutes))

(|*) :: Integer -> CalendarDiffDays -> CalendarDiffDays
n |* dur = scaleCalendarDiffDays n dur
infixl 7 |*

duration :: Text -> Parser Integer
duration x = option 0 ((skipSpace *> decimal) <* (skipSpace *> optionalPlural x))

optionalPlural :: Text -> Parser Text
optionalPlural x = (string x) <* (option 's' $ char 's')
