module System.Xml.XmlDeclaration where

import DotNet
import qualified System.Xml.XmlLinkedNode
import qualified System.Xml.XmlWriter
import qualified System.Xml.XmlNodeTy
import qualified System.Xml.XmlNodeType

data XmlDeclaration_ a
type XmlDeclaration a = System.Xml.XmlLinkedNode.XmlLinkedNode (XmlDeclaration_ a)

foreign import dotnet
  "method System.Xml.XmlDeclaration.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDeclaration obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDeclaration.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDeclaration obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDeclaration.set_InnerText"
  set_InnerText :: String -> XmlDeclaration obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_InnerText"
  get_InnerText :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_LocalName"
  get_LocalName :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.CloneNode"
  cloneNode :: Bool -> XmlDeclaration obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_NodeType"
  get_NodeType :: XmlDeclaration obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlDeclaration.set_Value"
  set_Value :: String -> XmlDeclaration obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_Value"
  get_Value :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_Name"
  get_Name :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_Version"
  get_Version :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_Encoding"
  get_Encoding :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.set_Encoding"
  set_Encoding :: String -> XmlDeclaration obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDeclaration.get_Standalone"
  get_Standalone :: XmlDeclaration obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDeclaration.set_Standalone"
  set_Standalone :: String -> XmlDeclaration obj -> IO (())


