module System.Xml.XmlDocumentTy where

import DotNet
import System.Xml.XmlNodeTy

data XmlDocument_ a
type XmlDocument a = System.Xml.XmlNodeTy.XmlNode (XmlDocument_ a)

