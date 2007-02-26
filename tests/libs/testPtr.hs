-- !!! Testing Ptr arithmetic and shows
import Foreign.Ptr

testShowAndPlus :: IO ()
testShowAndPlus = sequence_ [putStrLn (show64 (nullPtr `plusPtr` i)) | i <- [-100..100]]

show64 :: Ptr a -> String
show64 p =
   case show p of
      '0':'x':xs -> '0':'x':pseudoSignExtend xs

pseudoSignExtend :: String -> String
pseudoSignExtend xs =
   case length xs of
      8 -> replicate 8 (head xs) ++ xs
      16 -> xs
