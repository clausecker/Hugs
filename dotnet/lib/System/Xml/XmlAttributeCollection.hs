module System.Xml.XmlAttributeCollection 
	( module System.Xml.XmlAttributeCollection ,
	  module System.Xml.XmlAttributeCollectionTy
	) where

import DotNet
import qualified System.Xml.XmlAttributeCollectionTy
import qualified System.Xml.XmlNamedNodeMap
import qualified System.Xml.XmlAttributeTy
import qualified System.Xml.XmlNodeTy
import qualified System.Array

data XmlAttributeCollection_ a
type XmlAttributeCollection a = System.Xml.XmlNamedNodeMap.XmlNamedNodeMap (XmlAttributeCollection_ a)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.RemoveAll"
  removeAll :: XmlAttributeCollection obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.RemoveAt"
  removeAt :: Int -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.Remove"
  remove :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.InsertAfter"
  insertAfter :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> System.Xml.XmlAttributeTy.XmlAttribute a1 -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.InsertBefore"
  insertBefore :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> System.Xml.XmlAttributeTy.XmlAttribute a1 -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.Append"
  append :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.Prepend"
  prepend :: System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf :: String -> String -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf_1 :: String -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf_2 :: Int -> XmlAttributeCollection obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.SetNamedItem"
  setNamedItem :: System.Xml.XmlNodeTy.XmlNode a0 -> XmlAttributeCollection obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlAttributeCollection.CopyTo"
  copyTo :: System.Array.Array (System.Xml.XmlAttributeTy.XmlAttribute a0) -> Int -> XmlAttributeCollection obj -> IO (())


