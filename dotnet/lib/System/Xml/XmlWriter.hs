module System.Xml.XmlWriter where

import DotNet
import qualified System.Object
import System.Xml.XmlReader
import System.Xml.XmlSpace
import System.Xml.WriteState
import qualified System.Array
import System.Byte
import qualified System.Char

data XmlWriter_ a
type XmlWriter a = System.Object.Object (XmlWriter_ a)

foreign import dotnet
  "method System.Xml.XmlWriter.WriteNode"
  writeNode :: System.Xml.XmlReader.XmlReader a0 -> Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteAttributes"
  writeAttributes :: System.Xml.XmlReader.XmlReader a0 -> Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteQualifiedName"
  writeQualifiedName :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteName"
  writeName :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteNmToken"
  writeNmToken :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.get_XmlLang"
  get_XmlLang :: XmlWriter obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlWriter.get_XmlSpace"
  get_XmlSpace :: XmlWriter obj -> IO (System.Xml.XmlSpace.XmlSpace a0)

foreign import dotnet
  "method System.Xml.XmlWriter.LookupPrefix"
  lookupPrefix :: String -> XmlWriter obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlWriter.Flush"
  flush :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.Close"
  close :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.get_WriteState"
  get_WriteState :: XmlWriter obj -> IO (System.Xml.WriteState.WriteState a0)

foreign import dotnet
  "method System.Xml.XmlWriter.WriteBinHex"
  writeBinHex :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteBase64"
  writeBase64 :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteRaw"
  writeRaw :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteRaw"
  writeRaw_1 :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteChars"
  writeChars :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteSurrogateCharEntity"
  writeSurrogateCharEntity :: Char -> Char -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteString"
  writeString :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteWhitespace"
  writeWhitespace :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteCharEntity"
  writeCharEntity :: Char -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteEntityRef"
  writeEntityRef :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteProcessingInstruction"
  writeProcessingInstruction :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteComment"
  writeComment :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteCData"
  writeCData :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteEndAttribute"
  writeEndAttribute :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartAttribute"
  writeStartAttribute :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteFullEndElement"
  writeFullEndElement :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteEndElement"
  writeEndElement :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartElement"
  writeStartElement :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteDocType"
  writeDocType :: String -> String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteEndDocument"
  writeEndDocument :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartDocument"
  writeStartDocument :: Bool -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartDocument"
  writeStartDocument_1 :: XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartElement"
  writeStartElement_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartElement"
  writeStartElement_2 :: String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString :: String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteAttributeString"
  writeAttributeString_2 :: String -> String -> String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteStartAttribute"
  writeStartAttribute_1 :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteElementString"
  writeElementString :: String -> String -> XmlWriter obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlWriter.WriteElementString"
  writeElementString_1 :: String -> String -> String -> XmlWriter obj -> IO (())


