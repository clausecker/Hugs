#ifndef CALLBACKS_H
#define CALLBACKS_H

typedef void (*displayCB_t)(void);

extern void registerDisplayCB(displayCB_t);
extern void invokeDisplayCB(void);

#endif
