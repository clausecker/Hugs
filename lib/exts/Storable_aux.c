#include "HsFFI.h"

void writeIntOffPtr(HsPtr arg1, HsInt arg2, HsInt arg3)
{
  ((typeof(arg3)*)arg1)[arg2] = arg3;
}

HsInt readIntOffPtr(HsPtr arg1, HsInt arg2)
{
  return ((typeof(readIntOffPtr(arg1,arg2))*)arg1)[arg2];
}

void writeCharOffPtr(HsPtr arg1, HsInt arg2, HsChar arg3)
{
  ((typeof(arg3)*)arg1)[arg2] = arg3;
}

HsChar readCharOffPtr(HsPtr arg1, HsInt arg2)
{
  return ((typeof(readCharOffPtr(arg1,arg2))*)arg1)[arg2];
}

