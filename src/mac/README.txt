
    Compiling for Mac Symantec C/C++ v8r5.
            Hans Aberg

* Source code newlines should be converted to Mac newlines.

* The Symantec project window should have the following files added:
    src/mac/
        launch.c
    Runtime Libraries:
        InterfaceLib
        MathLib
        PPCANSI.o
        PPCRuntime.o
        PPCunix.o
    src/
        builtin.c
        compiler.c
        hugs.c
        input.c
        machine.c
        output.c
        plugin.c
        static.c
        storage.c
        type.c

* Remove all subdirectories of src/, except src/mac/, which may contain
  the files config.h or options.h, or enclose their names with parenthises
  "()", in order to prevent the compiler reading them.

* Compiler:
    Language settings: Relaxed ANSI conformance.
    Prefix: None (MacHeaders causes name conflicts with Hugs).


    
