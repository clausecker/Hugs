#pragma once

extern System::Object* getNP(Cell c);

extern "C" {
Cell   mkDotNetPtr     (System::Object *, Void (*)(System::Object *));
Void incDotNetPtrRefCnt(Int, Int);
Void freeNetPtr (System::Object* x);
};
