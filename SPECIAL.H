#ifdef ATARI
 #include <process.h>        /*fuer exec*/
 #include <ext.h>            /*fuer cd*/
 #include <tos.h>            /*fuer pwd*/


 int cd(char *newdir)
 /* change directory */
 /* 0:success*/
 {
  return chdir(newdir);
 }

 void pwd(void)
 /*print working directory*/
 {
  char path[64];
  if (Dgetpath(path,0)==0)
  printf("%c:%s\n",Dgetdrv()+'A',path);
  else printf("unable to get working directory\n");   
 }
#endif

#ifdef PC
 #include <process.h>        /*fuer exec*/
/* #include <dir.h> */           /*fuer cd und pwd (borland turbo c)*/
 #include <direct.h>	     /*fuer cd und pwd (msvc)*/
 #include <errno.h>
 #include <signal.h>
 #include <setjmp.h>

 typedef void (*fptr)();

 extern unsigned _stklen=64000U;  /*Groesse des STACKS*/
 jmp_buf reenter;                /*Einsprungpunkt bei FPU-Fehler*/

 void float_trap(int sig)
 {
    printf("ERROR! Your FPU or FPU emulator is angry (usually division by zero).\n");
    printf("       Sorry. The only thing you can do now is to restart GNUTIL.\n");
 /*
    reset the 8087 chip or emulator to clear
    any extraneous garbage
 */
    _fpreset();
 /* return to the problem spot */
    longjmp(reenter, -1);
 }
 /* Achtung! Der Handler muss mit SIGNAL in COMMAND.C gesetzt werden!!!*/

 int cd(char *newdir)
 /* change directory */
 /* 0:success*/
 {
  return chdir(newdir);
 }

 void pwd(void)
 /*print working directory*/
 {
  char path[128];
  getcwd(path,128);
  printf("%s\n",path);
 }

 int exec(char *name, char *command, char *environment, int *returncode)
 {
  int ret;
  ret=spawnlp(P_WAIT,name,name,command,NULL);
  return ret;
 }
 /*definition nicht vorhandener Funktionen aus math.h*/

 double atanh(double x)
 {
  return 0.5*log((1+x)/(1-x));
 }

 double asinh(double x)
 {
  return log(x+sqrt(x*x+1));
 }

 double acosh(double x)
 {
  if (x==1) return 0.0;
  else return log(x+sqrt(x*x-1));
 }

#endif

#ifdef RS6000
#endif
