--
-- Implementation of Haskell 98's Time module. Suitable for use
-- with Hugs98.
--
module Time
	( ClockTime(..)         -- non-standard, report says its abstract.
				-- instances: Eq, Ord
				-- instances: Show  (non-standard)
	, Month( January
	       , February
	       , March
	       , April
	       , May
	       , June
	       , July
	       , August
	       , September
	       , October
	       , November
	       , December
	       )		-- instances: Eq, Ord, Enum, Bounded
	       			-- instances: Ix, Read, Show
	, Day( Sunday
	     , Monday
	     , Tuesday
	     , Wednesday
	     , Thursday
	     , Friday
	     , Saturday)	-- instances: Eq, Ord, Enum, Bounded
	     			-- instances: Ix, Read, Show
	, CalendarTime( CalendarTime
		      , ctYear
		      , ctMonth
		      , ctDay
		      , ctHour
		      , ctMin
		      , ctPicosec
		      , ctWDay
		      , ctYDay
		      , ctTZName
		      , ctTZ
		      , ctIsDST
		      )		-- instances: Eq, Ord, Read, Show
	, TimeDiff( TimeDiff
		  , tdYear
		  , tdMonth
		  , tdDay
		  , tdHour
		  , tdMin
		  , tdSec
		  , tdPicosec
		  )		-- instances: Eq, Ord, Read, Show

	, getClockTime		-- :: IO ClockTime
	, addToClockTime	-- :: TimeDiff  -> ClockTime -> ClockTime
	, diffClockTimes	-- :: ClockTime -> ClockTime -> TimeDiff

	, toCalendarTime	-- :: ClockTime -> IO CalendarTime
	, toUTCTime		-- :: ClockTime -> CalendarTime
	, toClockTime		-- :: CalendarTime -> ClockTime

	, calendarTimeToString  -- :: CalendarTime -> String
	, formatCalendarTime    -- :: TimeLocale -> String -> CalendarTime -> String
	
	  -- NON-STANDARD (but also provided by GHC impl)
	, noTimeDiff	        -- :: TimeDiff
	) where

import Locale
import IOExts

data ClockTime 
  = ClockTime Integer  -- secs
  	      Integer  -- micro (10^-6) secs [0, 999999]
    deriving ( Eq, Ord )


-- Definitions of Month, Day, ClockTime, TimeDiff - straight out of the report:
data Month 
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
    deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday 
  | Friday
  | Saturday
    deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data CalendarTime 
  = CalendarTime 
        { ctYear                          :: Int
	, ctMonth                         :: Month
	, ctDay, ctHour, ctMin, ctSec     :: Int
	, ctPicosec                       :: Integer
	, ctWDay                          :: Day
	, ctYDay                          :: Int
	, ctTZName                        :: String
	, ctTZ                            :: Int
	, ctIsDST                         :: Bool
        } deriving (Eq, Ord, Read, Show)

data TimeDiff 
  = TimeDiff 
        { tdYear, tdMonth, tdDay :: Int
	, tdHour, tdMin, tdSec   :: Int
	, tdPicosec              :: Integer
        } deriving (Eq, Ord, Read, Show)

noTimeDiff=TimeDiff 0 0 0
		    0 0 0
		    0

getClockTime :: IO ClockTime
getClockTime = do
  (s,micros) <- getClockTimePrim 
  return (ClockTime (fromIntegral s) (fromIntegral micros))

primitive getClockTimePrim :: IO (Int,Int)

toClockTime :: CalendarTime -> ClockTime
toClockTime (CalendarTime yr mon mday
			  hour min sec _
			  _ _ tzname tz isdst) = unsafePerformIO $ do
  s <- toClockTimePrim (yr-1900) (fromEnum mon) mday
  		       hour min sec
		       tz (if isdst then 1 else 0)
  return (ClockTime (fromIntegral s) 0)

primitive toClockTimePrim :: Int -> Int -> Int
			  -> Int -> Int -> Int
			  -> Int -> Int -> IO Int

toUTCTime :: ClockTime -> CalendarTime
toUTCTime = toCalTime True

toCalendarTime :: ClockTime -> CalendarTime
toCalendarTime = toCalTime False

toCalTime :: Bool -> ClockTime -> CalendarTime
toCalTime toUTC (ClockTime s msecs)
  | (s > fromIntegral (maxBound :: Int)) || 
    (s < fromIntegral (minBound :: Int))
  = error ((if toUTC then "toUTCTime: " else "toCalendarTime: ") ++
           "clock secs out of range")
  | otherwise = unsafePerformIO $ do
    (sec,min,hour,mday,mon,year,wday,yday,isdst,zone,off) <- 
  		toCalTimePrim (if toUTC then 1 else 0) (fromIntegral s)
    return (CalendarTime{ ctYear=1900+year
  		        , ctMonth=toEnum mon
		        , ctDay=mday
		        , ctHour=hour
		        , ctMin=min
		        , ctSec=sec
		        , ctPicosec=msecs*1000*1000
		        , ctWDay=toEnum wday
		        , ctYDay=yday
		        , ctTZName=(if toUTC then "UTC" else zone)
		        , ctTZ=(if toUTC then 0 else off)
		        , ctIsDST=not toUTC && (isdst/=0)
		        })

primitive toCalTimePrim :: Int -> Int -> IO (Int,Int,Int,Int,Int,Int,Int,Int,Int,String,Int)

-- non-standard Show instance, but worth it..? (provided by GHC too).
instance Show ClockTime where
  show ct = calendarTimeToString (toCalendarTime ct)

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec)
	       (ClockTime csecs msecs) = 
	let
	  (r_yr, r_mon) = mon `quotRem` 12
	  
	  psecToMSec ps = ps `div` 1000000

	  secOff = 
	    toInteger sec + 60 * toInteger min +
	    3600 * toInteger hour + 24 * 3600 * toInteger day + exSec
	  (exSec,msecs') = (msecs + psecToMSec psec) `quotRem` 999999
	  
	  new_mon = fromEnum (ctMonth cal) + r_mon
	  (month', yr_diff) 
	    | new_mon <  0 = (toEnum (12 + new_mon), -1)
	    | new_mon > 11 = (toEnum (new_mon `mod` 12), 1)
	    | otherwise    = (toEnum new_mon, 0)

	  year' = ctYear cal + year + r_yr + yr_diff
	  cal = toUTCTime(ClockTime (csecs + secOff) msecs')
	in
	toClockTime cal{ctMonth=month', ctYear=year'}

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (ClockTime s1 ms1) (ClockTime s2 ms2) = 
	noTimeDiff{ tdSec=fromIntegral(s1-s2)
		  , tdPicosec= 1000 * 1000 * fromIntegral(ms1-ms2)
		  }

-- formatting CalendarTimes.
calendarTimeToString    :: CalendarTime -> String
calendarTimeToString    =  formatCalendarTime defaultTimeLocale "%c"

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt ct@(CalendarTime year mon day hour min sec sdec 
                                           wday yday tzname _ _) =
        doFmt fmt
  where doFmt ('%':c:cs) = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        decode 'A' = fst (wDays l  !! fromEnum wday)
        decode 'a' = snd (wDays l  !! fromEnum wday)
        decode 'B' = fst (months l !! fromEnum mon)
        decode 'b' = snd (months l !! fromEnum mon)
        decode 'h' = snd (months l !! fromEnum mon)
        decode 'C' = show2 (year `quot` 100)
        decode 'c' = doFmt (dateTimeFmt l)
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day
        decode 'e' = show2' day
        decode 'H' = show2 hour
        decode 'I' = show2 (to12 hour)
        decode 'j' = show3 yday
        decode 'k' = show2' hour
        decode 'l' = show2' (to12 hour)
        decode 'M' = show2 min
        decode 'm' = show2 (fromEnum mon+1)
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l)
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec
        decode 's' = show (case toClockTime ct of { (ClockTime s _) -> s })
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7)
        decode 'u' = show (let n = fromEnum wday in 
                           if n == 0 then 7 else n)
        decode 'V' = 
            let (week, days) = 
                   (yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1 
                       else if week == 0 then 53 else week)

        decode 'W' = 
            show2 ((yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)
        decode 'X' = doFmt (timeFmt l)
        decode 'x' = doFmt (dateFmt l)
        decode 'Y' = show year
        decode 'y' = show2 (year `rem` 100)
        decode 'Z' = tzname
        decode '%' = "%"
        decode c   = [c]

show2, show2', show3 :: Int -> String
show2 x = [intToDigit (x `quot` 10), intToDigit (x `rem` 10)]

show2' x = if x < 10 then [ ' ', intToDigit x] else show2 x

show3 x = intToDigit (x `quot` 100) : show2 (x `rem` 100)

to12 :: Int -> Int
to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'

