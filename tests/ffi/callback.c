#include "callback.h"

static displayCB_t theDisplayCB;

void registerDisplayCB(displayCB_t cb)
{
  theDisplayCB = cb;
}

void invokeDisplayCB(void)
{
  theDisplayCB();
}
