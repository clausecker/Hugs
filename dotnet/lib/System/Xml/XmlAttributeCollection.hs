module System.Xml.XmlAttributeCollection where

import DotNet
import qualified System.Object
import qualified System.Xml.XmlNamedNodeMap

data XmlAttributeCollection_ a

type XmlAttributeCollection a = System.Xml.XmlNamedNodeMap (XmlAttributeCollection_ a)

--itemOf :: String -> XmlAttributeCollection a -> IO (XmlAttribute b)

-- etc.

