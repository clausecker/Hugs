module System.Xml.XmlNodeType where

import DotNet
import qualified System.Enum
import qualified IOExts
import qualified System.Type
import qualified System.Enum

data XmlNodeType_ a
type XmlNodeType a = System.Enum.Enum (XmlNodeType_ a)

data XmlNodeTypeTy
 = Value__
 | None
 | Element
 | Attribute
 | Text
 | CDATA
 | EntityReference
 | Entity
 | ProcessingInstruction
 | Comment
 | Document
 | DocumentType
 | DocumentFragment
 | Notation
 | Whitespace
 | SignificantWhitespace
 | EndElement
 | EndEntity
 | XmlDeclaration
  deriving ( Enum, Show, Read )
toXmlNodeType :: XmlNodeTypeTy -> XmlNodeType ()
toXmlNodeType tag = IOExts.unsafePerformIO (System.Enum.parse (IOExts.unsafePerformIO (System.Type.getType "System.Xml.XmlNodeType, System.Xml, Version=1.0.3300.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")) (show tag))

fromXmlNodeType :: XmlNodeType () -> XmlNodeTypeTy
fromXmlNodeType obj = IOExts.unsafePerformIO (toString obj >>= return.read)

