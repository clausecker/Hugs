module System.MarshalByRefObject where

import DotNet
import qualified System.Object
import System.Type
import System.Runtime.Remoting.ObjRef

data MarshalByRefObject_ a
type MarshalByRefObject a = System.Object.Object (MarshalByRefObject_ a)

createObjRef :: System.Type.Type a0 -> MarshalByRefObject obj -> IO (System.Runtime.Remoting.ObjRef a1)
createObjRef arg0  = invoke "CreateObjRef" arg0
initializeLifetimeService :: MarshalByRefObject obj -> IO (System.Object.Object a0)
initializeLifetimeService  = invoke "InitializeLifetimeService" ()
getLifetimeService :: MarshalByRefObject obj -> IO (System.Object a0)
getLifetimeService  = invoke "GetLifetimeService" ()

