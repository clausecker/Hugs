module System.Xml.XmlText where

import DotNet
import qualified System.Xml.XmlCharacterData
import System.Xml.XmlWriter
import System.Xml.XmlNodeTy
import System.Xml.XmlNodeType

data XmlText_ a
type XmlText a = System.Xml.XmlCharacterData.XmlCharacterData (XmlText_ a)

foreign import dotnet
  "method System.Xml.XmlText.SplitText"
  splitText :: Int -> XmlText obj -> IO (System.Xml.XmlText.XmlText a1)

foreign import dotnet
  "method System.Xml.XmlText.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlText obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlText.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlText obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlText.get_LocalName"
  get_LocalName :: XmlText obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlText.CloneNode"
  cloneNode :: Bool -> XmlText obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlText.get_NodeType"
  get_NodeType :: XmlText obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlText.set_Value"
  set_Value :: String -> XmlText obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlText.get_Value"
  get_Value :: XmlText obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlText.get_Name"
  get_Name :: XmlText obj -> IO (String)


