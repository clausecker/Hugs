module System.Xml.XmlElementTy where

import DotNet
import System.Xml.XmlLinkedNode

data XmlElement_ a
type XmlElement a = System.Xml.XmlLinkedNode.XmlLinkedNode (XmlElement_ a)

