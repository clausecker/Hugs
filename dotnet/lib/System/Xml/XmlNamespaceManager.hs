module System.Xml.XmlNamespaceManager where

import DotNet
import qualified System.Object
import System.Collections.IEnumerator
import System.Xml.XmlNameTable

data XmlNamespaceManager_ a
type XmlNamespaceManager a = System.Object.Object (XmlNamespaceManager_ a)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.GetEnumerator"
  getEnumerator :: XmlNamespaceManager obj -> IO (System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.HasNamespace"
  hasNamespace :: String -> XmlNamespaceManager obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.LookupPrefix"
  lookupPrefix :: String -> XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.LookupNamespace"
  lookupNamespace :: String -> XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.RemoveNamespace"
  removeNamespace :: String -> String -> XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.AddNamespace"
  addNamespace :: String -> String -> XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.PopScope"
  popScope :: XmlNamespaceManager obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.PushScope"
  pushScope :: XmlNamespaceManager obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.get_DefaultNamespace"
  get_DefaultNamespace :: XmlNamespaceManager obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlNamespaceManager.get_NameTable"
  get_NameTable :: XmlNamespaceManager obj -> IO (System.Xml.XmlNameTable.XmlNameTable a0)


