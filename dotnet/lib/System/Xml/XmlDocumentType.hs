module System.Xml.XmlDocumentType where

import DotNet
import qualified System.Xml.XmlLinkedNode
import System.Xml.XmlWriter
import System.Xml.XmlNode
import System.Xml.XmlNodeType
import System.Xml.XmlNamedNodeMap

data XmlDocumentType_ a
type XmlDocumentType a = System.Xml.XmlLinkedNode.XmlLinkedNode (XmlDocumentType_ a)

foreign import dotnet
  "method System.Xml.XmlDocumentType.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentType obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocumentType.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentType obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_IsReadOnly"
  get_IsReadOnly :: XmlDocumentType obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_LocalName"
  get_LocalName :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocumentType.CloneNode"
  cloneNode :: Bool -> XmlDocumentType obj -> IO (System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_NodeType"
  get_NodeType :: XmlDocumentType obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_Name"
  get_Name :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_Entities"
  get_Entities :: XmlDocumentType obj -> IO (System.Xml.XmlNamedNodeMap.XmlNamedNodeMap a0)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_Notations"
  get_Notations :: XmlDocumentType obj -> IO (System.Xml.XmlNamedNodeMap.XmlNamedNodeMap a0)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_PublicId"
  get_PublicId :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_SystemId"
  get_SystemId :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocumentType.get_InternalSubset"
  get_InternalSubset :: XmlDocumentType obj -> IO (String)


