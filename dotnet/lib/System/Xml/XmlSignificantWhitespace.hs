module System.Xml.XmlSignificantWhitespace where

import DotNet
import qualified System.Xml.XmlCharacterData
import System.Xml.XmlWriter
import System.Xml.XmlNodeTy
import System.Xml.XmlNodeType

data XmlSignificantWhitespace_ a
type XmlSignificantWhitespace a = System.Xml.XmlCharacterData.XmlCharacterData (XmlSignificantWhitespace_ a)

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.get_LocalName"
  get_LocalName :: XmlSignificantWhitespace obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.CloneNode"
  cloneNode :: Bool -> XmlSignificantWhitespace obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.get_NodeType"
  get_NodeType :: XmlSignificantWhitespace obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.set_Value"
  set_Value :: String -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.get_Value"
  get_Value :: XmlSignificantWhitespace obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlSignificantWhitespace.get_Name"
  get_Name :: XmlSignificantWhitespace obj -> IO (String)


