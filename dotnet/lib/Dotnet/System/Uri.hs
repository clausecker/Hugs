module Dotnet.System.Uri where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import Dotnet.System.UriHostNameType
import Dotnet.System.Array
import Dotnet.System.String
import Dotnet.System.UriPartial
import Dotnet.System.Int32

data Uri_ a
type Uri a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (Uri_ a)

foreign import dotnet
  "method Dotnet.System.Uri.get_AbsolutePath"
  get_AbsolutePath :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_AbsoluteUri"
  get_AbsoluteUri :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Authority"
  get_Authority :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Fragment"
  get_Fragment :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Host"
  get_Host :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_HostNameType"
  get_HostNameType :: Uri obj -> IO (Dotnet.System.UriHostNameType.UriHostNameType a0)

foreign import dotnet
  "method Dotnet.System.Uri.get_IsDefaultPort"
  get_IsDefaultPort :: Uri obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.get_IsFile"
  get_IsFile :: Uri obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.get_IsLoopback"
  get_IsLoopback :: Uri obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.get_IsUnc"
  get_IsUnc :: Uri obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.get_LocalPath"
  get_LocalPath :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_PathAndQuery"
  get_PathAndQuery :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Port"
  get_Port :: Uri obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Uri.get_Query"
  get_Query :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Scheme"
  get_Scheme :: Uri obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Uri.get_Segments"
  get_Segments :: Uri obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a0))

foreign import dotnet
  "method Dotnet.System.Uri.get_UserEscaped"
  get_UserEscaped :: Uri obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.get_UserInfo"
  get_UserInfo :: Uri obj -> IO (String)

foreign import dotnet
  "static method Dotnet.System.Uri.CheckHostName"
  checkHostName :: String -> IO (Dotnet.System.UriHostNameType.UriHostNameType a1)

foreign import dotnet
  "static method Dotnet.System.Uri.CheckSchemeName"
  checkSchemeName :: String -> IO (Bool)

foreign import dotnet
  "static method Dotnet.System.Uri.FromHex"
  fromHex :: Char -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Uri.GetLeftPart"
  getLeftPart :: Dotnet.System.UriPartial.UriPartial a0 -> Uri obj -> IO (String)

foreign import dotnet
  "static method Dotnet.System.Uri.HexEscape"
  hexEscape :: Char -> IO (String)

foreign import dotnet
  "static method Dotnet.System.Uri.HexUnescape"
  hexUnescape :: String -> Dotnet.System.Int32.Int32 a1 -> IO (Char)

foreign import dotnet
  "static method Dotnet.System.Uri.IsHexDigit"
  isHexDigit :: Char -> IO (Bool)

foreign import dotnet
  "static method Dotnet.System.Uri.IsHexEncoding"
  isHexEncoding :: String -> Int -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Uri.MakeRelative"
  makeRelative :: Dotnet.System.Uri.Uri a0 -> Uri obj -> IO (String)


