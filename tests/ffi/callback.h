#ifndef CALLBACK_H
#define CALLBACK_H

typedef void (*displayCB_t)(void);

extern void registerDisplayCB(displayCB_t);
extern void invokeDisplayCB(void);

#endif
