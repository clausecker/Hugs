module System.Xml.XmlNode (module System.Xml.XmlNode, module XmlNodeTy) where

import DotNet
import qualified System.Xml
import qualified System.Xml.XmlAttributeCollection
import XmlNodeTy

attributes :: XmlNode a -> IO (System.Xml.XmlAttributeCollection.XmlAttributeCollection b)
attributes = invoke "get_Attributes" ()

baseURI :: XmlNode a -> IO String
baseURI = invoke "get_BaseURI" ()




