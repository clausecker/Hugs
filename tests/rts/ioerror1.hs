-- !!! Testing IOError (part 1)

import IO

-- printing IOError values
a1 = userError "foo"

-- testing IOError values
a2 = isUserError (userError "foo")

-- catching IOErrors
a3 = catch (ioError (userError "foo")) (\err -> putStr "Caught error\n")

-- continuing after catching errors
a4 = catch (ioError (userError "foo")) (\err -> putStr "Caught error\n") >>
     putStr "Continuing\n"

-- raising uncaught errors
a5 :: IO () -- signature required to override "IO a"
a5 = ioError (userError "foo")
