module System.Xml.XmlAttributeCollectionTy where

import System.Xml.XmlNamedNodeMap

data XmlAttributeCollection_ a
type XmlAttributeCollection a = System.Xml.XmlNamedNodeMap.XmlNamedNodeMap (XmlAttributeCollection_ a)

