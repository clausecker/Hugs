module System.Xml.XmlEntityReference where

import DotNet
import qualified System.Xml.XmlLinkedNode
import System.Xml.XmlWriter
import System.Xml.XmlNode
import System.Xml.XmlNodeType

data XmlEntityReference_ a
type XmlEntityReference a = System.Xml.XmlLinkedNode.XmlLinkedNode (XmlEntityReference_ a)

foreign import dotnet
  "method System.Xml.XmlEntityReference.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlEntityReference.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_BaseURI"
  get_BaseURI :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_IsReadOnly"
  get_IsReadOnly :: XmlEntityReference obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_LocalName"
  get_LocalName :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlEntityReference.CloneNode"
  cloneNode :: Bool -> XmlEntityReference obj -> IO (System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_NodeType"
  get_NodeType :: XmlEntityReference obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlEntityReference.set_Value"
  set_Value :: String -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_Value"
  get_Value :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlEntityReference.get_Name"
  get_Name :: XmlEntityReference obj -> IO (String)


