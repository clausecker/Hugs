--
-- Implementation of Haskell 98's Locale module
-- 
module Locale
	( TimeLocale(..)
	, defaultTimeLocale  -- :: Locale
	) where

data TimeLocale 
 = TimeLocale 
       { wDays       :: [(String, String)]   -- full and abbreviated week days
       , months      :: [(String, String)]   -- full and abbreviated months
       , amPm        :: (String, String)     -- AM/PM symbols
       , dateTimeFmt :: String
       , dateFmt     :: String
       , timeFmt     :: String
       , time12Fmt   :: String
       } deriving (Eq, Ord, Show)

defaultTimeLocale :: TimeLocale 
defaultTimeLocale 
  =  TimeLocale 
  	 { wDays  = map (\ x -> (x, take 3 x)) days_english
	 , months = map (\ x -> (x, take 3 x)) months_english
         , amPm = ("AM", "PM")
         , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
         , dateFmt = "%m/%d/%y"
         , timeFmt = "%H:%M:%S"
         , time12Fmt = "%I:%M:%S %p"
         }

days_english :: [String]
days_english
 = [ "Sunday"
   , "Monday"
   , "Tuesday"
   , "Wednesday"
   , "Thursday"
   , "Friday"
   , "Saturday"
   ]
   
months_english :: [String]
months_english
 = [ "January"
   , "February"
   , "March"
   , "April"
   , "May"
   , "June"
   , "July"
   , "August"
   , "September"
   , "October"
   , "November"
   , "December"
   ]

