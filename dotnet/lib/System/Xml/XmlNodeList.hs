module System.Xml.XmlNodeList where

import DotNet
import qualified System.Object
import System.Collections.IEnumerator
import System.Xml.XmlNodeTy

data XmlNodeList_ a
type XmlNodeList a = System.Object.Object (XmlNodeList_ a)

foreign import dotnet
  "method System.Xml.XmlNodeList.GetEnumerator"
  getEnumerator :: XmlNodeList obj -> IO (System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method System.Xml.XmlNodeList.get_ItemOf"
  get_ItemOf :: Int -> XmlNodeList obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlNodeList.get_Count"
  get_Count :: XmlNodeList obj -> IO (Int)

foreign import dotnet
  "method System.Xml.XmlNodeList.Item"
  item :: Int -> XmlNodeList obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)


