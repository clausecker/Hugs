module System.Uri where

import DotNet
import qualified System.MarshalByRefObject
import System.UriHostNameType
import System.Array
import System.String
import System.UriPartial
import System.Int32

data Uri_ a
type Uri a = System.MarshalByRefObject.MarshalByRefObject (Uri_ a)

foreign import dotnet
  "method System.Uri.get_AbsolutePath"
  get_AbsolutePath :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_AbsoluteUri"
  get_AbsoluteUri :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Authority"
  get_Authority :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Fragment"
  get_Fragment :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Host"
  get_Host :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_HostNameType"
  get_HostNameType :: Uri obj -> IO (System.UriHostNameType.UriHostNameType a0)

foreign import dotnet
  "method System.Uri.get_IsDefaultPort"
  get_IsDefaultPort :: Uri obj -> IO (Bool)

foreign import dotnet
  "method System.Uri.get_IsFile"
  get_IsFile :: Uri obj -> IO (Bool)

foreign import dotnet
  "method System.Uri.get_IsLoopback"
  get_IsLoopback :: Uri obj -> IO (Bool)

foreign import dotnet
  "method System.Uri.get_IsUnc"
  get_IsUnc :: Uri obj -> IO (Bool)

foreign import dotnet
  "method System.Uri.get_LocalPath"
  get_LocalPath :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_PathAndQuery"
  get_PathAndQuery :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Port"
  get_Port :: Uri obj -> IO (Int)

foreign import dotnet
  "method System.Uri.get_Query"
  get_Query :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Scheme"
  get_Scheme :: Uri obj -> IO (String)

foreign import dotnet
  "method System.Uri.get_Segments"
  get_Segments :: Uri obj -> IO (System.Array.Array (System.String.StringTy a0))

foreign import dotnet
  "method System.Uri.get_UserEscaped"
  get_UserEscaped :: Uri obj -> IO (Bool)

foreign import dotnet
  "method System.Uri.get_UserInfo"
  get_UserInfo :: Uri obj -> IO (String)

foreign import dotnet
  "static method System.Uri.CheckHostName"
  checkHostName :: String -> IO (System.UriHostNameType.UriHostNameType a1)

foreign import dotnet
  "static method System.Uri.CheckSchemeName"
  checkSchemeName :: String -> IO (Bool)

foreign import dotnet
  "static method System.Uri.FromHex"
  fromHex :: Char -> IO (Int)

foreign import dotnet
  "method System.Uri.GetLeftPart"
  getLeftPart :: System.UriPartial.UriPartial a0 -> Uri obj -> IO (String)

foreign import dotnet
  "static method System.Uri.HexEscape"
  hexEscape :: Char -> IO (String)

foreign import dotnet
  "static method System.Uri.HexUnescape"
  hexUnescape :: String -> System.Int32.Int32 a1 -> IO (Char)

foreign import dotnet
  "static method System.Uri.IsHexDigit"
  isHexDigit :: Char -> IO (Bool)

foreign import dotnet
  "static method System.Uri.IsHexEncoding"
  isHexEncoding :: String -> Int -> IO (Bool)

foreign import dotnet
  "method System.Uri.MakeRelative"
  makeRelative :: System.Uri.Uri a0 -> Uri obj -> IO (String)


