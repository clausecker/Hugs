module System.Xml.XmlNamedNodeMap where

import DotNet
import qualified System.Object

data XmlNamedNodeMap_ a

type XmlNamedNodeMap a = System.Object.Object (XmlNamedNodeMap_ a)


count :: XmlNamedNodeMap a -> IO Int
count = invoke "get_Count" ()

getNamedItem :: String -> XmlNamedNodeMap a -> IO (System.Xml.XmlNodeTy.XmlNode b)
getNamedItem nm = invoke "GetNamedItem" nm

getNamedItemURI :: String -> String -> XmlNamedNodeMap a -> IO (System.Xml.XmlNodeTy.XmlNode b)
getNamedItemURI nm uri = invoke "GetNamedItem" (nm,uri)

item :: Int -> XmlNamedNodeMap a -> IO (System.Xml.XmlNodeTy.XmlNode b)
item idx = invoke "Item" idx

removeNamedItem :: String -> XmlNamedNodeMap a -> IO (System.Xml.XmlNodeTy.XmlNode b)
removeNamedItem nm = invoke "RemoveNamedItem" nm

removeNamedItemURI :: String -> String -> XmlNamedNodeMap a -> IO (System.Xml.XmlNodeTy.XmlNode b)
removeNamedItemURI nm uri = invoke "RemoveNamedItem" (nm,uri)

setNamedItem :: System.Xml.XmlNodeTy.XmlNode a -> XmlNamedNodeMap b -> IO (System.Xml.XmlNodeTy.XmlNode c)
