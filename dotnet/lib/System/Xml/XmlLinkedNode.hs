module System.Xml.XmlLinkedNode where

import DotNet
import qualified System.Xml.XmlNodeTy
import System.Xml.XmlNodeTy

data XmlLinkedNode_ a
type XmlLinkedNode a = System.Xml.XmlNodeTy.XmlNode (XmlLinkedNode_ a)

foreign import dotnet
  "method System.Xml.XmlLinkedNode.get_NextSibling"
  get_NextSibling :: XmlLinkedNode obj -> IO (System.Xml.XmlNodeTy.XmlNode a0)

foreign import dotnet
  "method System.Xml.XmlLinkedNode.get_PreviousSibling"
  get_PreviousSibling :: XmlLinkedNode obj -> IO (System.Xml.XmlNodeTy.XmlNode a0)


