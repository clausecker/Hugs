module System.Xml.XmlAttributeTy where

import DotNet
import System.Xml.XmlNodeTy

data XmlAttribute_ a
type XmlAttribute a = System.Xml.XmlNodeTy.XmlNode (XmlAttribute_ a)

