module System.Xml.XmlNameTable where

import DotNet
import qualified System.Object

data XmlNameTable_ a
type XmlNameTable a = System.Object.Object (XmlNameTable_ a)

add :: String -> XmlNameTable a -> IO String
add str = invoke "Add" str

get :: String -> XmlNameTable a -> IO String
get str = invoke "Get" str
