module System.Xml.XmlImplementation where

import DotNet
import qualified System.Object
import System.Xml.XmlDocumentTy

data XmlImplementation_ a
type XmlImplementation a = System.Object.Object (XmlImplementation_ a)

foreign import dotnet
  "method System.Xml.XmlImplementation.CreateDocument"
  createDocument :: XmlImplementation obj -> IO (System.Xml.XmlDocumentTy.XmlDocument a0)

foreign import dotnet
  "method System.Xml.XmlImplementation.HasFeature"
  hasFeature :: String -> String -> XmlImplementation obj -> IO (Bool)


