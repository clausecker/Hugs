module System.Xml.XmlCDataSection where

import DotNet
import qualified System.Xml.XmlCharacterData
import System.Xml.XmlWriter
import System.Xml.XmlNode
import System.Xml.XmlNodeType

data XmlCDataSection_ a
type XmlCDataSection a = System.Xml.XmlCharacterData.XmlCharacterData (XmlCDataSection_ a)

foreign import dotnet
  "method System.Xml.XmlCDataSection.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlCDataSection obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlCDataSection.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlCDataSection obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlCDataSection.get_LocalName"
  get_LocalName :: XmlCDataSection obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlCDataSection.CloneNode"
  cloneNode :: Bool -> XmlCDataSection obj -> IO (System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlCDataSection.get_NodeType"
  get_NodeType :: XmlCDataSection obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlCDataSection.get_Name"
  get_Name :: XmlCDataSection obj -> IO (String)


