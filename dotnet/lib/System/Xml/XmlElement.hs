module System.Xml.XmlElement 
	( module System.Xml.XmlElement ,
	  module System.Xml.XmlElementTy
	) where

import DotNet
import qualified System.Xml.XmlLinkedNode
import System.Xml.XmlElementTy
import System.Xml.XmlNodeTy
import System.Xml.XmlNodeList
import System.Xml.XmlAttributeTy
import System.Xml.XmlWriter
import System.Xml.XmlDocument
import System.Xml.XmlAttributeCollectionTy
import System.Xml.XmlNodeType

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAllAttributes"
  removeAllAttributes :: XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAttributeAt"
  removeAttributeAt :: Int -> XmlElement obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlElement.HasAttribute"
  hasAttribute :: String -> String -> XmlElement obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlElement.HasAttribute"
  hasAttribute_1 :: String -> XmlElement obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlElement.GetElementsByTagName"
  getElementsByTagName :: String -> String -> XmlElement obj -> IO (System.Xml.XmlNodeList.XmlNodeList a2)

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAttributeNode"
  removeAttributeNode :: String -> String -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlElement.SetAttributeNode"
  setAttributeNode :: String -> String -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlElement.GetAttributeNode"
  getAttributeNode :: String -> String -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAttribute"
  removeAttribute :: String -> String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.SetAttribute"
  setAttribute :: String -> String -> String -> XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.GetAttribute"
  getAttribute :: String -> String -> XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.GetElementsByTagName"
  getElementsByTagName_1 :: String -> XmlElement obj -> IO (System.Xml.XmlNodeList.XmlNodeList a1)

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAttributeNode"
  removeAttributeNode_1 :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlElement.SetAttributeNode"
  setAttributeNode_1 :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlElement.GetAttributeNode"
  getAttributeNode_1 :: String -> XmlElement obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAttribute"
  removeAttribute_1 :: String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.SetAttribute"
  setAttribute_1 :: String -> String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.GetAttribute"
  getAttribute_1 :: String -> XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.get_HasAttributes"
  get_HasAttributes :: XmlElement obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlElement.RemoveAll"
  removeAll :: XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.set_InnerXml"
  set_InnerXml :: String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.get_InnerXml"
  get_InnerXml :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.set_InnerText"
  set_InnerText :: String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.get_InnerText"
  get_InnerText :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.get_LocalName"
  get_LocalName :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.set_Prefix"
  set_Prefix :: String -> XmlElement obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlElement.get_Prefix"
  get_Prefix :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.get_NamespaceURI"
  get_NamespaceURI :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.CloneNode"
  cloneNode :: Bool -> XmlElement obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlElement.get_OwnerDocument"
  get_OwnerDocument :: XmlElement obj -> IO (System.Xml.XmlDocument.XmlDocument a0)

foreign import dotnet
  "method System.Xml.XmlElement.get_Attributes"
  get_Attributes :: XmlElement obj -> IO (System.Xml.XmlAttributeCollectionTy.XmlAttributeCollection a0)

foreign import dotnet
  "method System.Xml.XmlElement.get_NextSibling"
  get_NextSibling :: XmlElement obj -> IO (System.Xml.XmlNodeTy.XmlNode a0)

foreign import dotnet
  "method System.Xml.XmlElement.get_NodeType"
  get_NodeType :: XmlElement obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlElement.get_Name"
  get_Name :: XmlElement obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlElement.get_IsEmpty"
  get_IsEmpty :: XmlElement obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlElement.set_IsEmpty"
  set_IsEmpty :: Bool -> XmlElement obj -> IO (())


