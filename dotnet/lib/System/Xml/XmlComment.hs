module System.Xml.XmlComment where

import DotNet
import qualified System.Xml.XmlCharacterData
import System.Xml.XmlWriter
import System.Xml.XmlNode
import System.Xml.XmlNodeType

data XmlComment_ a
type XmlComment a = System.Xml.XmlCharacterData.XmlCharacterData (XmlComment_ a)

foreign import dotnet
  "method System.Xml.XmlComment.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlComment obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlComment.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlComment obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlComment.get_LocalName"
  get_LocalName :: XmlComment obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlComment.CloneNode"
  cloneNode :: Bool -> XmlComment obj -> IO (System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlComment.get_NodeType"
  get_NodeType :: XmlComment obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlComment.get_Name"
  get_Name :: XmlComment obj -> IO (String)


