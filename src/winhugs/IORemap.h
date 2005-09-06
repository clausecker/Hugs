
// standard definitions for communicating with WinHugs deliberately
// pop up the message in an error box
void ErrorBox(const char* Msg);
void InfoBox(const char* Msg);

void WinHugsExit();

extern int InAutoReloadFiles;
void SetWorkingDir(const char* Src);
void startEvaluatorThread();
void loopInBackground();

// Used for trapping console output and GUI'ifying it
// fprintf (stdstr, ...) is used to direct output to the string stdstrbuff
extern FILE *stdstr;
extern char stdstrbuff[];

// Colours

// do not use Windows RGB so you don't introduce a dependancy on Windows.h
#define rgb(r,g,b) (r | (g << 8) | (b << 16))

#define BLACK           rgb(0,0,0)
#define BLUE            rgb(0,0,175)
#define GREEN           rgb(0,135,0)
#define CYAN            rgb(0,175,175)
#define RED             rgb(175,0,0)
#define MAGENTA         rgb(150,0,150)
#define YELLOW          rgb(175,175,0)
#define WHITE           rgb(255,255,255)

void WinHugsPutS(FILE* f, char* Buffer);
int WinHugsPrintf(const char* format, ...);
int WinHugsFPrintf(FILE* f, const char* format, ...);
int WinHugsPutC(FILE* f, char c);
int WinHugsGetC(FILE* f);
int WinHugsColor(int Color);
void WinHugsHyperlink(const char* msg);

// Add support for Most Recently Used Files
void WinHugsAddMruFile(const char* file);
void WinHugsMessagePump();

// undefine everything that is a macro already
#undef getc
#undef getchar
#undef putchar
#undef putc

// output with formatting buffers
#define printf          WinHugsPrintf
#define fprintf         WinHugsFPrintf

// standard output
#define putchar(ch)     WinHugsPutC(stdout, ch)
#define putc(ch,file)   WinHugsPutC(file, ch)
#define fputc(ch,file)  WinHugsPutC(file, ch)

// standard input
#define getc(file)      WinHugsGetC(file)
#define getchar()       WinHugsGetC(stdin)
#define getch()         WinHugsGetC(stdin)

// unused?
#define kbhit()         WinKbhit(hWndText)
