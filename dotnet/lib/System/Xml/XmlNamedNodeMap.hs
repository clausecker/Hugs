module System.Xml.XmlNamedNodeMap where

import DotNet
import qualified System.Object
import System.Collections.IEnumerator
import System.Xml.XmlNodeTy

data XmlNamedNodeMap_ a
type XmlNamedNodeMap a = System.Object.Object (XmlNamedNodeMap_ a)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.GetEnumerator"
  getEnumerator :: XmlNamedNodeMap obj -> IO (System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.RemoveNamedItem"
  removeNamedItem :: String -> String -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.GetNamedItem"
  getNamedItem :: String -> String -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.Item"
  item :: Int -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.get_Count"
  get_Count :: XmlNamedNodeMap obj -> IO (Int)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.RemoveNamedItem"
  removeNamedItem_1 :: String -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.SetNamedItem"
  setNamedItem :: System.Xml.XmlNodeTy.XmlNode a0 -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlNamedNodeMap.GetNamedItem"
  getNamedItem_1 :: String -> XmlNamedNodeMap obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)


