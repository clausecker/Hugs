module System.Xml.XmlNodeTy where

import qualified System.Object

data XmlNode_ a
type XmlNode a = System.Object.Object (XmlNode_ a)
