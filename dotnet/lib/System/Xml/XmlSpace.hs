module System.Xml.XmlSpace where

import DotNet
import qualified System.Enum

data XmlSpace_ a
type XmlSpace a = System.Enum.Enum (XmlSpace_ a)


