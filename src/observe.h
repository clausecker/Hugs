/*
 * Debugging via observations.
 *
 * Note: only available via the command interpreter, not
 *       in batch mode.
 *
 */
#ifndef __OBSERVE_H__
#define __OBSERVE_H__

#ifndef HUGS_SERVER
EXT_PROTO_PRIM(primObserve);
EXT_PROTO_PRIM(primBkpt);
EXT_PROTO_PRIM(primSetBkpt);
#else
/* Provide stubbed out primops in non-interactive modes. Tiresome. */
#include "builtin.h"
#include "evaluator.h"
#include "errors.h"
extPrimFun(primObserve) {
   ERRMSG(0) "'observe' primop only supported in interactive mode"
   EEND;
}
extPrimFun(primBkpt) {
   ERRMSG(0) "'bkpt' primop only supported in interactive mode"
   EEND;
}
extPrimFun(primSetBkpt) {
   ERRMSG(0) "'bkpt' primop only supported in interactive mode"
   EEND;
}
#endif

#endif /* __OBSERVE_H__ */
