--
-- External parsing of XML documents
--
module Xml where

import Dotnet
import qualified Dotnet.System.Xml.XmlDocument
import Dotnet.System.Xml.XmlNode
import qualified Dotnet.System.Xml.XmlNodeType as Type
import Dotnet.System.Xml.XmlNodeList
import qualified Dotnet.System.Xml.XmlAttributeCollection as Attr
import qualified Dotnet.System.Xml.XmlNamedNodeMap as Attr
import qualified Dotnet.System.Xml.XmlAttribute as At
import XMLSyn
import Maybe

--
-- This example demonstrates how to make use of the .NET Xml classes
-- to handle the parsing of XML documents. After having parsed a document
-- externally, we simply walk over the document to generate a Haskell
-- representation of the document. 
--

loadXML :: String -> IO XMLDoc
loadXML url = do
  doc <- newDoc
  doc # Dotnet.System.Xml.XmlDocument.load_3 url
  l   <- doc # get_FirstChild
  tag <- doc # get_NodeType
  let v = Type.fromXmlNodeType tag
  case v of
    Type.Document -> do
      vs <- doc # getNodes
      return (XMLDoc Nothing vs)
    _ -> return (XMLDoc Nothing [])

getNodes :: XmlNode a -> IO [Markup]
getNodes node = do
      ls <- node # get_ChildNodes
      c  <- ls   # get_Count
      vs <- mapM (\ i -> ls # item i >>= \ obj -> getNode obj) [0..(c-1)]
      return (catMaybes vs)

getNode :: XmlNode a -> IO (Maybe Markup)
getNode node = do
  tag <- node # get_NodeType
  let v = Type.fromXmlNodeType tag
  case v of
    Type.Element -> do
      s  <- node # get_Name
      vs <- node # getNodes
      as <- node # getAttributes
      return (Just (XMLSyn.Element (Elem s as (Just vs))))
    Type.Comment -> do
      s <- node # get_InnerText
      return (Just (XMLSyn.Comment s))
    Type.Text -> do
      s <- node # get_InnerText
      return (Just (XMLSyn.CharData s))
    _ -> do
      str <- toString tag
      print str
      return Nothing

getAttributes :: XmlNode a -> IO [Attribute]
getAttributes node = do
  as <- node # get_Attributes
  c  <- as # Attr.get_Count
  mapM (\ i -> as # Attr.item i >>= \ obj -> getAttribute obj) [0..(c-1)]
  
getAttribute :: At.XmlAttribute a -> IO Attribute
getAttribute attr = do
  x <- attr # At.get_LocalName
  y <- attr # At.get_Value
  return (Attribute x y)

foreign import dotnet
  "ctor System.Xml.XmlDocument"
  newDoc :: IO (Dotnet.System.Xml.XmlDocument.XmlDocument ())
