module System.Xml.XmlWhitespace where

import DotNet
import qualified System.Xml.XmlCharacterData
import System.Xml.XmlWriter
import System.Xml.XmlNodeTy
import System.Xml.XmlNodeType

data XmlWhitespace_ a
type XmlWhitespace a = System.Xml.XmlCharacterData.XmlCharacterData (XmlWhitespace_ a)

foreign import dotnet
  "method System.Xml.XmlWhitespace.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWhitespace.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWhitespace.get_LocalName"
  get_LocalName :: XmlWhitespace obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlWhitespace.CloneNode"
  cloneNode :: Bool -> XmlWhitespace obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlWhitespace.get_NodeType"
  get_NodeType :: XmlWhitespace obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlWhitespace.set_Value"
  set_Value :: String -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWhitespace.get_Value"
  get_Value :: XmlWhitespace obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlWhitespace.get_Name"
  get_Name :: XmlWhitespace obj -> IO (String)


