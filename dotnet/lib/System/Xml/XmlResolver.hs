module System.Xml.XmlResolver where

import DotNet
import qualified System.Object
--import System.Net.ICredentials
import System.Uri
import System.Type

data XmlResolver_ a
type XmlResolver a = System.Object.Object (XmlResolver_ a)

{-
foreign import dotnet
  "method System.Xml.XmlResolver.set_Credentials"
  set_Credentials :: System.Net.ICredentials.ICredentials a0 -> XmlResolver obj -> IO (())
-}

foreign import dotnet
  "method System.Xml.XmlResolver.ResolveUri"
  resolveUri :: System.Uri.Uri a0 -> String -> XmlResolver obj -> IO (System.Uri.Uri a2)

foreign import dotnet
  "method System.Xml.XmlResolver.GetEntity"
  getEntity :: System.Uri.Uri a0 -> String -> System.Type.Type a2 -> XmlResolver obj -> IO (System.Object.Object a3)


