module System.Xml.XmlProcessingInstruction where

import DotNet
import qualified System.Xml.XmlLinkedNode
import System.Xml.XmlWriter
import System.Xml.XmlNode
import System.Xml.XmlNodeType

data XmlProcessingInstruction_ a
type XmlProcessingInstruction a = System.Xml.XmlLinkedNode.XmlLinkedNode (XmlProcessingInstruction_ a)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlProcessingInstruction obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlProcessingInstruction obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.set_InnerText"
  set_InnerText :: String -> XmlProcessingInstruction obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_InnerText"
  get_InnerText :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_LocalName"
  get_LocalName :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.CloneNode"
  cloneNode :: Bool -> XmlProcessingInstruction obj -> IO (System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_NodeType"
  get_NodeType :: XmlProcessingInstruction obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.set_Value"
  set_Value :: String -> XmlProcessingInstruction obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_Value"
  get_Value :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_Name"
  get_Name :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_Target"
  get_Target :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.get_Data"
  get_Data :: XmlProcessingInstruction obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlProcessingInstruction.set_Data"
  set_Data :: String -> XmlProcessingInstruction obj -> IO (())


