module System.Xml.XmlDocument 
	( module System.Xml.XmlDocument ,
	  module System.Xml.XmlDocumentTy
	) where

import DotNet
import qualified System.Xml.XmlNode
import qualified System.Xml.XmlDocumentTy
import System.Xml.XmlWriter
import System.IO.TextWriter
import System.IO.Stream
import System.Xml.XmlReader
import System.IO.TextReader
import System.Xml.XmlNodeTy
import System.Xml.XmlNodeType
import System.Xml.XmlElementTy
import System.Xml.XmlAttributeTy
import System.Xml.XmlNodeList
import System.Xml.XmlWhitespace
import System.Xml.XmlSignificantWhitespace
import System.Xml.XmlText
import System.Xml.XmlDeclaration
import System.Xml.XmlProcessingInstruction
import System.Xml.XmlEntityReference
import System.Xml.XmlDocumentFragment
import System.Xml.XmlDocumentType
import System.Xml.XmlComment
import System.Xml.XmlCDataSection
import System.Xml.XmlResolver
import System.Xml.XmlImplementation
import System.Xml.XmlNameTable
--import System.Xml.XmlNodeChangedEventHandler

data XmlDocument_ a
type XmlDocument a = System.Xml.XmlNodeTy.XmlNode (XmlDocument_ a)

foreign import dotnet
  "method System.Xml.XmlDocument.Save"
  save :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Save"
  save_1 :: System.IO.TextWriter.TextWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Save"
  save_2 :: System.IO.Stream.Stream a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Save"
  save_3 :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.LoadXml"
  loadXml :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Load"
  load :: System.Xml.XmlReader.XmlReader a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Load"
  load_1 :: System.IO.TextReader.TextReader a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Load"
  load_2 :: System.IO.Stream.Stream a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.Load"
  load_3 :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.ReadNode"
  readNode :: System.Xml.XmlReader.XmlReader a0 -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateNode"
  createNode :: System.Xml.XmlNodeType.XmlNodeType a0 -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a3)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateNode"
  createNode_1 :: String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a3)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateNode"
  createNode_2 :: System.Xml.XmlNodeType.XmlNodeType a0 -> String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a4)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateElement"
  createElement :: String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlElementTy.XmlElement a3)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateAttribute"
  createAttribute :: String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a3)

foreign import dotnet
  "method System.Xml.XmlDocument.ImportNode"
  importNode :: System.Xml.XmlNodeTy.XmlNode a0 -> Bool -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method System.Xml.XmlDocument.GetElementById"
  getElementById :: String -> XmlDocument obj -> IO (System.Xml.XmlElementTy.XmlElement a1)

foreign import dotnet
  "method System.Xml.XmlDocument.GetElementsByTagName"
  getElementsByTagName :: String -> String -> XmlDocument obj -> IO (System.Xml.XmlNodeList.XmlNodeList a2)

foreign import dotnet
  "method System.Xml.XmlDocument.GetElementsByTagName"
  getElementsByTagName_1 :: String -> XmlDocument obj -> IO (System.Xml.XmlNodeList.XmlNodeList a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateWhitespace"
  createWhitespace :: String -> XmlDocument obj -> IO (System.Xml.XmlWhitespace.XmlWhitespace a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateSignificantWhitespace"
  createSignificantWhitespace :: String -> XmlDocument obj -> IO (System.Xml.XmlSignificantWhitespace.XmlSignificantWhitespace a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateTextNode"
  createTextNode :: String -> XmlDocument obj -> IO (System.Xml.XmlText.XmlText a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateXmlDeclaration"
  createXmlDeclaration :: String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlDeclaration.XmlDeclaration a3)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateProcessingInstruction"
  createProcessingInstruction :: String -> String -> XmlDocument obj -> IO (System.Xml.XmlProcessingInstruction.XmlProcessingInstruction a2)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateEntityReference"
  createEntityReference :: String -> XmlDocument obj -> IO (System.Xml.XmlEntityReference.XmlEntityReference a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateDocumentFragment"
  createDocumentFragment :: XmlDocument obj -> IO (System.Xml.XmlDocumentFragment.XmlDocumentFragment a0)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateDocumentType"
  createDocumentType :: String -> String -> String -> String -> XmlDocument obj -> IO (System.Xml.XmlDocumentType.XmlDocumentType a4)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateComment"
  createComment :: String -> XmlDocument obj -> IO (System.Xml.XmlComment.XmlComment a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateCDataSection"
  createCDataSection :: String -> XmlDocument obj -> IO (System.Xml.XmlCDataSection.XmlCDataSection a1)

foreign import dotnet
  "method System.Xml.XmlDocument.set_XmlResolver"
  set_XmlResolver :: System.Xml.XmlResolver.XmlResolver a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.get_DocumentType"
  get_DocumentType :: XmlDocument obj -> IO (System.Xml.XmlDocumentType.XmlDocumentType a0)

foreign import dotnet
  "method System.Xml.XmlDocument.WriteContentTo"
  writeContentTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.WriteTo"
  writeTo :: System.Xml.XmlWriter.XmlWriter a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.get_BaseURI"
  get_BaseURI :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocument.set_InnerXml"
  set_InnerXml :: String -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.get_InnerXml"
  get_InnerXml :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocument.get_IsReadOnly"
  get_IsReadOnly :: XmlDocument obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlDocument.get_LocalName"
  get_LocalName :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocument.CloneNode"
  cloneNode :: Bool -> XmlDocument obj -> IO (System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method System.Xml.XmlDocument.get_OwnerDocument"
  get_OwnerDocument :: XmlDocument obj -> IO (System.Xml.XmlDocument.XmlDocument a0)

foreign import dotnet
  "method System.Xml.XmlDocument.get_NodeType"
  get_NodeType :: XmlDocument obj -> IO (System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method System.Xml.XmlDocument.get_Name"
  get_Name :: XmlDocument obj -> IO (String)

foreign import dotnet
  "method System.Xml.XmlDocument.get_Implementation"
  get_Implementation :: XmlDocument obj -> IO (System.Xml.XmlImplementation.XmlImplementation a0)

foreign import dotnet
  "method System.Xml.XmlDocument.get_DocumentElement"
  get_DocumentElement :: XmlDocument obj -> IO (System.Xml.XmlElementTy.XmlElement a0)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateAttribute"
  createAttribute_1 :: String -> XmlDocument obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateElement"
  createElement_1 :: String -> XmlDocument obj -> IO (System.Xml.XmlElementTy.XmlElement a1)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateAttribute"
  createAttribute_2 :: String -> String -> XmlDocument obj -> IO (System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method System.Xml.XmlDocument.CreateElement"
  createElement_2 :: String -> String -> XmlDocument obj -> IO (System.Xml.XmlElementTy.XmlElement a2)

foreign import dotnet
  "method System.Xml.XmlDocument.get_NameTable"
  get_NameTable :: XmlDocument obj -> IO (System.Xml.XmlNameTable.XmlNameTable a0)

foreign import dotnet
  "method System.Xml.XmlDocument.get_PreserveWhitespace"
  get_PreserveWhitespace :: XmlDocument obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XmlDocument.set_PreserveWhitespace"
  set_PreserveWhitespace :: Bool -> XmlDocument obj -> IO (())

{-
foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeInserting"
  add_NodeInserting :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeInserting"
  remove_NodeInserting :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeInserted"
  add_NodeInserted :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeInserted"
  remove_NodeInserted :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeRemoving"
  add_NodeRemoving :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeRemoving"
  remove_NodeRemoving :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeRemoved"
  add_NodeRemoved :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeRemoved"
  remove_NodeRemoved :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeChanging"
  add_NodeChanging :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeChanging"
  remove_NodeChanging :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.add_NodeChanged"
  add_NodeChanged :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())

foreign import dotnet
  "method System.Xml.XmlDocument.remove_NodeChanged"
  remove_NodeChanged :: System.Xml.XmlNodeChangedEventHandler.XmlNodeChangedEventHandler a0 -> XmlDocument obj -> IO (())


-}
