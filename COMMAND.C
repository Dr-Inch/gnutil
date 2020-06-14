#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>          /*fuer isdigit*/
/*#define ATARI*/
#define PC
/*#define RS6000*/

#include "newton.h"
/*f_scan.h, gauss.h und special.h werden in newton.h geholt*/

#define COMLEN 255   /*Kommando max. 255 char*/
#define EINGLEN 800 /*Zeile max. 800 char*/
#define N_MAX 1000  /*max. 1000 Werte*/
#define PRECISION 15    /*15 Nachkommastellen*/

static void noquotes(char *s)   /*entfernt alle " und ' */
{
 int i,j,len;

 len=strlen(s);
 for (i=0; i<len; i++)
  {
   if ( (s[i]=='"') || (s[i]=='\'') )
     {
      len--;
      for (j=i; j<len; j++) 
        s[j]=s[j+1];
      i--;
     }
  }
 s[len]=0;
}

static void strdel(char *s, int first, int count)
/*loescht count Zeichen ab Position first aus dem String s*/
{
 int i,start,ende;
 start=first+count;
 ende=strlen(s)-start;
 if (ende<0) printf("ERROR: Bad Parameters in function strdel\n");
 else
  for (i=0; i<=ende; i++)
    s[first+i]=s[start+i];
}

static int anfang(char *s1, char *s2)
/*ergibt 1, wenn s1 der Anfang von s2 ist und loescht dann diesen*/
/*Anfang aus s2 heraus. Ansonsten wird Null zurueckgeliefert*/
{
 int l, erg;

 l=strlen(s1);
 erg=strncmp(s1,s2,l);
 if (erg) return 0;
 else 
  {
   strdel(s2,0,l); 
   return 1;
  }
}


static void getline(char *s)    /*hier komfortable Routine einfuegen*/
{
 char ss[EINGLEN];
 ss[0]='\\';
 ss[1]='\0';
 s[0]='\0';
 while (ss[strlen(ss)-1]=='\\')
  {
   gets(ss);
   if (strlen(ss)+strlen(s)<EINGLEN)
    {
     strcat(s,ss);      /*Anfuegen, falls '\' letztes Zeichen war*/
     if (s[strlen(s)-1]=='\\') s[strlen(s)-1]='\0';
    }
   else 
    {
     strncat(s,ss,EINGLEN-strlen(s));
     s[EINGLEN-1]='\0';
     printf("ERROR! Line too long, trying to proceed.\n");
     break;
    }
  }
}

/*hier Routinen fuer lineare Regression und Polynomfit*/

int lin(double *x, double *y, int n_val, 
        double *a, double *b, double *sa, double *sb, double *cor)
/*lineare Regression y=a+bx; 0=ok, -1=Fehler*/
{
 double sx,sy,sxy,sx2,sy2,sdy2,r,s,siy2;
 int i;

 printf("     Linear regression y=p0+p1*x.\n");
 if (n_val<3) 
  {
   printf("     ERROR. Minimum 3 values for linear regression.\n");
   return -1;
  }

 sx=0; sy=0;  sxy=0;  sx2=0;  sy2=0;  sdy2=0;
 for (i=0; i<n_val; i++)    /*Rechnung*/
  {
   r=x[i];
   s=y[i];
   sx=sx+r;          /*Regression*/
   sy=sy+s;
   sx2=sx2+r*r;
   sy2=sy2+s*s;
   sxy=sxy+r*s;
  } 
 *a=(sx2*sy-sx*sxy)/( ((double) n_val) *sx2-sx*sx); /*konstanter Term*/
 *b=( ((double) n_val) *sxy-sx*sy)/( ((double) n_val) *sx2-sx*sx);   /*linearer Term*/
 *cor=( ((double) n_val) *sxy-sx*sy)/sqrt(( ((double) n_val) *sx2-sx*sx)*( ((double) n_val) *sy2-sy*sy));
 for (i=0; i<n_val; i++)    /*Fehler*/
   sdy2=sdy2+pow( (y[i]-*a-*b*x[i]), 2.0);
 siy2=sdy2/( (double) (n_val-2) );
 *sa=sqrt(siy2*sx2/( ((double) n_val) *sx2-sx*sx));
 *sb=sqrt( siy2*((double) n_val) / ( ((double) n_val) *sx2-sx*sx) );

 printf("     Results: ");
 printf("p0=%lE+-%lE (68%%)\n",*a,*sa);
 printf("              p1=%lE+-%lE (68%%)\n",*b,*sb);
 printf("              correlation %lE\n",*cor);

 return 0;
}

int pol(int gd, double *x, double *y, int n_val, vector loesung)
/*polynom fit grad 2..9; Rueckgabe 0 fuer ok, -1 sonst*/
{
 double doppel[2*MAXGRAD];  /*Hilfsvektor*/
 vector rs;                 /*rechte Seite*/
 matrix gs;                 /*Gleichungssystem*/
 int i,j,k;                 /*Schleifen*/
 int ok;                    /*alles ok?*/

 printf ("     Polynom fit degree %d; y=p0+p1*x+p2*x**2+...\n",gd);
 if ( (gd<2) || (gd>9) )
  {
   printf("     ERROR. Degree must be 2..9.\n");
   return -1;
  }
 else 
  {
    for (i=0; i<gd+1; i++)          /*Gleichungssystem leeren*/
     {
      rs[i]=0.0;
      for (j=0; j<gd+1; j++) gs[i][j]=0.0;
     }
     
    for (j=0; j<n_val; j++)       /*Gleichungssystem auffuellen*/
     {
       doppel[0]=1.0;
       for (i=1; i<2*gd+1; i++)
          doppel[i]=doppel[i-1]*x[j];
          
       for (k=0; k<gd+1; k++)    /*Zeilenindex*/
        {
          rs[k]=rs[k]+doppel[k]*y[j];        /*rechte Seite fuellen*/
          for (i=0; i<gd+1; i++) /*Spaltenindex*/
            gs[k][i]+=doppel[i+k];           /*linke Seite fuellen*/
        }
     } 
   ok=gauss(gs,rs,gd+1,loesung);
   if (ok==0)
    {
     printf("     Results:\n");
     for (i=0; i<gd+1; i++)
      printf("          p%d=% lE\n",i,loesung[i]);
    }
   else if (ok==-1)
    printf("     ERROR! Polynom doesn't fit.\n");
   else if (ok==-2)
    printf("     ERROR! Can't calculate results (mathematical error).\n");
   return ok;
  }
}

int main(void)  
{
char eingabe[EINGLEN];      /*Eingabezeile*/
char lastfit[EINGLEN];      /*letztes fit-Kommando*/
char cmdfile[COMLEN];       /*Kommandofile relativ*/
char resfile[COMLEN];       /*Ergebnisfile relativ*/
char default_cmd[COMLEN];   /*default-Kommandofile absolut*/
char default_res[COMLEN];   /*default-Ergebnisfile absolut*/
char editor[COMLEN];        /*Name und Pfad des Editors*/
char gnuplot[COMLEN];       /*Name und Pfad von GNUPLOT*/

char com1[COMLEN], com2[COMLEN], com3[COMLEN], com4[COMLEN];
char com5[COMLEN], com6[COMLEN], com7[COMLEN]; /*max. 7 Argumente*/

FILE *fil1, *fil2;          /*Files*/
  
int dummy;                  /*dummy-int*/
int err;                    /*Hilfsvariable fuer Befehlsscanner*/
char helps1[EINGLEN], helps2[EINGLEN];  /*Hilfsstrings*/
double helpd;        	    /*Hilfsdouble*/
int i;                      /*Schleifenvariable*/
 
struct parblock start;      /*Startwert fuer Newton-Verfahren*/

double MACH_EPS;            /*Maschinengenauigkeit fuer double*/
{                           /*bestimme dieselbe*/
 double o,q;
 MACH_EPS = 1.0;
 o=2.0;
 q=1.0;
 while (q<o)
  {
   MACH_EPS *= 0.5;
   o=1.0+MACH_EPS;
  }
 MACH_EPS *= 2.0;
}


/*Meldung*/
printf("This is gnutil, version 2.02 (C-source) by Fons\n");
printf("Utility for evaluating and fitting ASCII data.\n\n");
printf("Type '?' or 'help' for a command list.\n\n\n");

/*Lesen des Info-Files*/
#ifdef ATARI
 fil1=fopen("gnutil.ins","r");
#endif
#ifdef PC
 /*fil1=fopen(searchpath ("gnutil.ins"),"r");*/
 fil1=fopen("gnutil.ins","r");
#endif
#ifdef RS6000
#endif
if (fil1==NULL)         /*Abbruch falls kein Info-File*/
 {
  printf("ERROR! Cannot find GNUTIL.INS!\n");
  exit(-1);
 }
gnuplot[0]='\0';
default_cmd[0]='\0';
editor[0]='\0';
while(fgets(eingabe,EINGLEN,fil1)!=NULL)
 {
  helps1[0]='\0';
  helps2[0]='\0';
  dummy=sscanf(eingabe,"%s%s",helps1,helps2);
  if (strcmp(helps1,"GNUPLOT:")==0) strcpy(gnuplot,helps2);
  if (strcmp(helps1,"COMMAND:")==0) strcpy(default_cmd,helps2);
  if (strcmp(helps1,"EDITOR:")==0) strcpy(editor,helps2);
  if (strcmp(helps1,"RESULTS:")==0) strcpy(default_res,helps2);
 }
if ( (gnuplot[0]=='\0') || (default_cmd[0]=='\0') || 
     (editor[0]=='\0') || (default_res[0]=='\0') )  /*Abbruch bei fehlendem Eintrag*/
 {
  printf("ERROR! Missing item in GNUTIL.INS!\n");
  exit(-1);
 }
/*Ende lesen des Info-Files*/

eingabe[0]='\0';    /*Vorbereiten*/
strcpy(lastfit,"useless command!");
for (i=0; i<=9; i++) start.p[i]=1.0;

while ( (strcmp(eingabe,"quit")!=0)     /*Hauptschleife*/
     && (strcmp(eingabe,"exit")!=0)     /*checke auf Programmende*/
     && (strcmp(eingabe,"q")!=0)    )
{

#ifdef PC
 /*
   install signal handler for floating point
   exception
 */
 signal(SIGFPE, (fptr)float_trap);
#endif
 
 err=0;                     /*kein Fehler*/
 printf("gnutil > ");       /*Eingabeaufforderung*/
 com1[0]='\0';          /*bereite Tokenfelder vor*/
 com2[0]='\0';
 com3[0]='\0';
 com4[0]='\0';
 com5[0]='\0';
 com6[0]='\0';
 com7[0]='\0';
 getline(eingabe);      /*lies Eingabe*/

reenter:

 noquotes(eingabe); /*entferne Anfuehrungszeichen (unnuetz, nur fuer*/
                    /*GNUPLOT Kompatibilitaet implementiert */

 dummy=sscanf(eingabe,"%s%s%s%s%s%s%s",com1,com2,com3,com4,com5,com6,com7);
 /*Scannen der Eingabezeile*/

/* printf("com1=%s\n",com1);    
 printf("com2=%s\n",com2);
 printf("com3=%s\n",com3);
 printf("com4=%s\n",com4);
 printf("com5=%s\n",com5);
 printf("com6=%s\n",com6);
 printf("com7=%s\n",com7);
 printf("Rueckgabewert%d\n",dummy); 
 puts(eingabe); */



 if (com1[0]=='\0') /*Keine Fehlermeldung bei leerer Eingabe*/
  ;
 else if (strcmp(com1,"refit")==0)  /*nochmal fitten*/
  {
   strcpy(eingabe,lastfit);
   goto reenter;
  }
 else if ( (strcmp(com1,"help")==0) || (strcmp(com1,"?")==0) )
  {                                 /*Hilfe anzeigen*/
   printf("Command list and syntax:\n\n");
/*   printf("<NAME>=<EXPRESSION>:\n");
   printf(" sets the variable NAME to the current value of EXPRESSION\n\n");*/
   printf("evaluate: (eval/e)\n");
   printf(" Syntax: evaluate <SOURCE> to <TARGET> x=<EXPRESSION> y=<EXPRESSION>\n");
   printf(" Evaluates the file SOURCE to the file TARGET by the given expressions.\n\n");
   printf("fit:\n");
   printf(" Syntax: fit {[min:max]} <SOURCE> <KIND> {-aACCURACY} {-rRESULTFILE} {-cCOMMANDFILE}\n");
   printf(" Fits (or at least tries to fit) a function specified by KIND through the\n");
   printf(" data specified in the SOURCE file within min to max. KIND may be:\n");
   printf(" lin (for linear regression), pol2..pol9 (for a polynom fit of degree 2..9)\n");
   printf(" or ANY sensible expression f(x,p0,..p9) where p0..p9 are the fit parameters.\n");
   printf(" In this case you MUST specify (sensible) starting values for all used\n");
   printf(" parameters by the SET command.\n");
   printf(" -a sets the fit-accuracy. Default is your maximum accuracy for doubles\n");
   printf(" -r redirects the fit results to RESULTFILE\n");
   printf(" -c writes GNUPLOT commands to the specified file for later use\n\n");
   printf("refit:\n");
   printf(" Syntax: refit\n");
   printf(" redoes last fit.\n\n");
   printf("press Return for more.\n");
   dummy=getchar();
   printf("set <PARAMETER> <VALUE>:\n");
   printf(" sets PARAMETER (p0..p9) to the specified starting VALUE.\n\n");
   printf("show:\n");
   printf(" Syntax: show {<COMMANDFILE>}\n");
   printf(" Shows the last fit results by calling GNUPLOT. If a COMMANDFILE is\n");
   printf(" specified, saved fit results will be shown.\n\n");
   printf("edit {<FILE>}:\n");
   printf(" Calls the Editor specified in GNUTIL.INS. Edits FILE if specified.\n\n");
   printf("gnuplot:\n");
   printf(" Calls GNUPLOT. Path and name must be given in GNUTIL.INS.\n\n");
   printf("exec <FILE> <COMMANDLINE>:\n");
   printf(" Calls the specified FILE for execution. COMMANDLINE is passed to FILE.\n\n");
   printf("system <COMMAND> or ! <COMMAND>:\n");
   printf(" Sends COMMAND to your commandline shell (if you do have any).\n\n");
   printf("press Return for more.\n");
   dummy=getchar();
   printf("pwd:\n");
   printf(" Print working directory.\n\n");
   printf("cd:\n");
   printf(" Change directory.\n\n");
   printf("exit or quit (q):\n");
   printf(" Exits this program.\n\n");
  }
 else if (strcmp(com1,"cd")==0)     /*Change Directory*/
  {
   if (com2[0]==0) printf("expecting directory name!\n");
   else if (cd(com2)!=0) printf("unknown directory!\n");
  }
 else if (strcmp(com1,"pwd")==0)    /*Print Working Directory*/
  {
   pwd();
  }
 else if (strcmp(com1,"edit")==0)   /*Editor aufrufen*/
  {
   if ( exec(editor,com2,"",&dummy)==-1 ) 
    printf("ERROR: Unable to call the editor!\n");
  }
 else if (strcmp(com1,"exec")==0)   /*Programm aufrufen*/
  {
   strcpy(helps1,com3);
   strcat(helps1," ");
   strcat(helps1,com4);
   strcat(helps1," ");
   strcat(helps1,com5);
   strcat(helps1," ");
   strcat(helps1,com6);
   if ( exec(com2,helps1,"",&dummy)==-1 ) 
    printf("ERROR: Unable to call %s!\n",com2);
  }
 else if (strcmp(com1,"gnuplot")==0)    /*Gnuplot aufrufen*/
  {
   if ( exec(gnuplot,com2,"",&dummy)==-1 ) 
    printf("ERROR: Unable to call gnuplot!\n");
  }
 else if (strcmp(com1,"show")==0)   /*mit GNUPLOT anzeigen*/
  {
   if (com2[0]=='\0')
    {
     if ( exec(gnuplot,default_cmd,"",&dummy)==-1 ) 
      printf("ERROR: Unable to call gnuplot!\n");
    }
   else
    {
     if ( exec(gnuplot,com2,"",&dummy)==-1 ) 
      printf("ERROR: Unable to call gnuplot!\n");
    }
  }
 else if ( (strcmp(com1,"system")==0)   /*Kommando an Betriebssystem*/
        || (strcmp(com1,"!")==0) )
  {
   strcpy(helps1,com2);
   strcat(helps1," ");
   strcat(helps1,com3);
   strcat(helps1," ");
   strcat(helps1,com4);
   strcat(helps1," ");
   strcat(helps1,com5);
   strcat(helps1," ");
   strcat(helps1,com6);
   if (system(NULL))
       system(helps1);
   else printf("ERROR! No command line interpreter found!\n"); 
  }
 else if (strcmp(com1,"set")==0)        /*Parameter setzen*/
  {
   char *end;

   helpd=strtod(com3,&end);
   if (  ( ((unsigned long) (end-com3)) != strlen(com3) )
       ||
         ( ((isdigit(com3[0])==0) && (com3[0]!='-') ) )
      ) 
     printf("ERROR! Not a number!\n");
   else
    {
     if ( (com2[0]!='p') || (com2[2]!='\0') )
       printf("ERROR: Valid arguments are p0..p9!\n");
     else
      {
       i=com2[1]-'0';
       if (isdigit(com2[1])==0)
         printf("ERROR: Valid arguments are p0..p9!\n");
       else start.p[i]=helpd;
      }
    }
  }
 else if ( (strcmp(com1,"evaluate")==0)   /*neues file berechnen*/
	 ||(strcmp(com1,"e")==0)
         ||(strcmp(com1,"eval")==0)    )
  {
  if (strcmp(com2,com4) != 0)
  {
   fil1=fopen(com2,"r");
   if (fil1!=NULL)
    {
     fil2=fopen(com4,"w");
     if (fil2!=NULL)
      {
       char z[EINGLEN];
       double x, y, y1, y2, xn, yn, y1n, y2n;
       struct f_node *fx, *fy, *dfyx, *dfyy;
       int cnt;
       struct parblock p;
    
       fprintf(fil2,"#File        : %s\n",com4);    /*Infos auf file*/
       fprintf(fil2,"#Created by  : GNUTIL 2.01\n");
       fprintf(fil2,"#Sourcefile  : %s\n",com2);
       fprintf(fil2,"#Expressions : %s\n",com5);
       fprintf(fil2,"#              %s\n",com6);
       fprintf(fil2,"#\n#\n#\n#\n#\n");
       
       if (anfang("x=",com5)==1) 
        {
         fx=scan(com5);
         if (fx==NULL) printf("ERROR! Bad expression x=%s!\n",com5);
        }
       else if (anfang("x=",com6)==1) 
        {
         fx=scan(com6);
         if (fx==NULL) printf("ERROR! Bad expression x=%s!\n",com5);
        }
       else
        fx=NULL;

       if (anfang("y=",com5)==1) 
        {
         fy=scan(com5);
         if (fy==NULL) 
          {
           printf("ERROR! Bad expression y=%s!\n",com5);
           dfyx=NULL; 
           dfyy=NULL;
          }
         else
          {
           dfyx=diff(fy,"x");   /*bilde partielle Ableitungen*/
           dfyy=diff(fy,"y");
          }
        }
       else if (anfang("y=",com6)==1) 
        {
         fy=scan(com6);
         if (fy==NULL) 
          {
           printf("ERROR! Bad expression y=%s!\n",com5);
           dfyx=NULL; 
           dfyy=NULL;
          }
         else
          {
           dfyx=diff(fy,"x");   /*bilde partielle Ableitungen*/
           dfyy=diff(fy,"y");
          }
        }
       else
        {
         fy=NULL;
         dfyx=NULL;
         dfyy=NULL;
        }
        
       if ( (fx!=NULL) || (fy!=NULL) )
        {
         if (fx==NULL)          /*kleiner Betrug*/
          fx=scan("x");
         if (fy==NULL)
          {
           fy=scan("y");
           dfyx=scan("0");
           dfyy=scan("1");
          }
         cnt=0;
         while( fgets(z,EINGLEN,fil1)!=NULL)    /*Werte einlesen*/
          {                                     /*und neu berechnen*/
           z[EINGLEN-1]='\0';
           cnt++;
           if (z[0]!='#')   /*kein Kommentar*/
            {
             x=HUGE_VAL;
             y=HUGE_VAL;
             y1=HUGE_VAL;
             y2=HUGE_VAL;
             sscanf(z,"%lf%lf%lf%lf",&x,&y,&y1,&y2);
             if ( (x!=HUGE_VAL) && (y!=HUGE_VAL) )
              {
               p.x=x;
               p.y=y;
               xn=eval(fx,&p);
               if (p.errflag==0)
                {
                 yn=eval(fy,&p);
                 if (p.errflag==0)
                  {
                   fprintf(fil2,"% .15lE    % .15lE",xn,yn);
                   if (y1!=HUGE_VAL)
                    {
                     if (y2==HUGE_VAL)
                      {
                       /*Auswertung des totalen Differentials*/
                       double df;

                       p.x=x;
                       p.y=y;
                       if ( (dfyx!=NULL) && (dfyy!=NULL) )
                        {
                         df=eval(dfyy,&p)*y1;
                         if ( p.errflag==0 )
                          fprintf(fil2,"    % .15lE\n",df);
                         else
                          {
                           fprintf(fil1,"\n");
                           printf("ERROR! Differential argument out of range or overflow");
                           printf("at line %d!\n",cnt);
                          }
                        }
                       else printf("ERROR! No differential. Errorbar ignored at line %d!\n",cnt);
                      }
                     else   /*Auswertung Ymin-Ymax*/
                      {
                       int e;

                       p.y=y1;
                       y1n=eval(fy,&p);
                       e=p.errflag;
                       p.y=y2;
                       y2n=eval(fy,&p);
                       if ( (p.errflag==0) && (e==0) )
                        {
                         fprintf(fil2,"    % .15lE    % .15lE\n",y1n,y2n);
                        }
                       else
                        {
                         printf ("ERROR! Errorbar(s) out of range or ");
                         printf("overflow at line %d! (forget about it)\n",cnt);
                         fprintf(fil2,"\n");
                        }
                      }
                    }
                   else fprintf(fil2,"\n");
                  }
                 else printf("ERROR! Y out of range or overflow in %s at line %d! (target left empty)\n",com5,cnt);
                }
               else printf("ERROR! X out of range or overflow in %s at line %d! (target left empty)\n",com5,cnt);
              }
             else printf("ERROR! Bad data in %s at line%d!\n",com5,cnt);
            }
          }
         wipe_function(fx); /*Gib Speicher frei*/
         wipe_function(fy);
         wipe_function(dfyx);
         wipe_function(dfyy);       
        }   
       else 
        printf("ERROR! Nothing to evaluate!\n");
       fclose(fil2);
      }
     else printf("ERROR! Unable to open file %s.\n",com4);
     fclose(fil1);
    }
   else printf("ERROR! Unable to open file %s.\n",com2);
  }
  else printf ("ERROR! Source- and targetfile may NEVER be identical!\n");
  } /*ende eval*/
 else if (strcmp(com1,"fit")==0)        /*fitten*/
  {
   struct parblock result;
   int cnt;
   int limited;
   double x_min,x_max;

   strcpy(lastfit,eingabe);	/*letzten Fit merken*/
   limited=0;
   x_min=-HUGE_VAL;   /*Bereich auf Maximum vorbelegen*/
   x_max=HUGE_VAL;
   if (com2[0]=='[')  /*Bereich eingeschraenkt?*/
    {
     printf("Limited range fit:\n");
     limited=1;
     com2[0]=' ';
     for (cnt=1; cnt<strlen(com2); cnt++)
      {
       if ( (com2[cnt]==':') || (com2[cnt]==']') )
        com2[cnt]=' ';
      }
     sscanf(com2,"%lf%lf",&x_min,&x_max);  /*setze Bereichsgrenzen*/
     strcpy(com2,com3);               /*rutsche Tokens durch*/
     strcpy(com3,com4);               /*(das ist zwar umstaendlich,*/
     strcpy(com4,com5);               /*aber diese Routine kam erst*/
     strcpy(com5,com6);               /*spaeter ins Programm)*/
     strcpy(com6,com7);
    }

   fil1=fopen(com2,"r");
   if (fil1!=NULL)
    {
     char z[EINGLEN];
     int n_val;
     static double x[N_MAX], y[N_MAX];
     int success;
     double accuracy;
     
     accuracy=MACH_EPS;
     strcpy(cmdfile,default_cmd);   /*defaults*/
     strcpy(resfile,default_res);

     if (anfang("-a",com4)==1) accuracy=strtod(com4,NULL);  /*setze Genauigkeit*/
     if (anfang("-a",com5)==1) accuracy=strtod(com5,NULL);  /*mit -a Option*/
     if (anfang("-a",com6)==1) accuracy=strtod(com6,NULL);
     if (accuracy==0.0) accuracy=MACH_EPS;  /*maximum bei fehlerhafter Eingabe*/

     if (anfang("-c",com4)==1) strcpy(cmdfile,com4);    /*setze Kommandofile*/
     if (anfang("-c",com5)==1) strcpy(cmdfile,com5);    /*mit -c Option*/
     if (anfang("-c",com6)==1) strcpy(cmdfile,com6);

     if (anfang("-r",com4)==1) strcpy(resfile,com4);    /*setze Ergebnisfile*/
     if (anfang("-r",com5)==1) strcpy(resfile,com5);    /*mit -r Option*/
     if (anfang("-r",com6)==1) strcpy(resfile,com6);


     n_val=0;
     cnt=0;
     while( fgets(z,EINGLEN,fil1)!=NULL)    /*Werte einlesen*/
      {
       z[EINGLEN-1]='\0';
       cnt++;
       if (z[0]!='#')   /*kein Kommentar*/
        {
         x[n_val]=HUGE_VAL;
         y[n_val]=HUGE_VAL;
         sscanf(z,"%lf%lf",&x[n_val],&y[n_val]);
         if ( (x[n_val]!=HUGE_VAL) && (y[n_val]!=HUGE_VAL) )
          {
           if ( (x[n_val]>=x_min) && (x[n_val]<=x_max) )
            n_val++;
/*printf("[%lf:%lf] n_val=%d  x=%lf\n",x_min,x_max,n_val,x[n_val-1]);*/
          }
         else printf("ERROR! Bad data in %s at line %d (ignored).\n",com2,cnt);

         if (n_val>=N_MAX)    
          {
           printf("ERROR! Maximum %d values (forget the rest).\n",N_MAX);
           break;
          }
        }
      } /*Ende Werte einlesen*/
     fclose(fil1);  /*file schliessen*/

     if ( strcmp(com3,"lin") ==0 )  /*lineare Regression*/
      {
       double a,b,sa,sb,cor;
       success=lin(x,y,n_val,&a,&b,&sa,&sb,&cor);
        /*lineare Regression y=a+bx; 0=ok, -1=Fehler*/
       if (success==0)
        {
         fil2=fopen(cmdfile,"w");   /*Kommandofile schreiben*/
         if (fil2!=NULL)
          {
           fprintf(fil2,"#File         : %s\n",cmdfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : linear\n");
           fprintf(fil2,"#Fit function : y=p0+p1*x\n");
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#\n#\n#\n#\n#\n");
           fprintf(fil2,"#Parameters:\n");
           fprintf(fil2,"p0=% .15lE\n",a);
           fprintf(fil2,"p1=% .15lE\n",b);
           fprintf(fil2,"#Fit function:\n");
           fprintf(fil2,"fit(x)=p0+p1*x\n");
           fprintf(fil2,"#\n#\n#\n");
           fprintf(fil2,"#plot it:\n");
           if (limited==1) 
            fprintf(fil2,"plot [%lf:%lf] '%s', fit(x)\n",x_min,x_max,com2);
           else
            fprintf(fil2,"plot '%s', fit(x)\n",com2);
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",cmdfile);

         fil2=fopen(resfile,"w");   /*Ergebnisfile schreiben*/
         if (fil2!=NULL)
          {
           fprintf(fil2,"#File         : %s\n",resfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : linear\n");
           fprintf(fil2,"#Fit function : y=p0+p1*x\n");
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#Results      : \n");
           fprintf(fil2,"               p0=% .15lE+-% .15lE (68%%)\n",a,sa);
           fprintf(fil2,"               p1=% .15lE+-% .15lE (68%%)\n",b,sb);
           fprintf(fil2,"                  (1-sigma Interval)\n");
           fprintf(fil2,"               correlation % .15lE\n",cor);
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",resfile);
        }
      }
     else if ( strncmp(com3,"pol",3) ==0 )  /*Polynom Fit*/
      {
       vector loesung;

       success=pol(com3[3]-'0',x,y,n_val,loesung);
       if (success==0)
        {
         int i,gd;

         gd=com3[3]-'0';
         fil2=fopen(cmdfile,"w");   /*Kommandofile schreiben*/
         if (fil2!=NULL)
          {
           fprintf(fil2,"#File         : %s\n",cmdfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : polynom, degree %d\n",gd);
           fprintf(fil2,"#Fit function : y=p0+p1*x+p2*x**2+...\n");
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#\n#\n#\n#\n#\n");
           fprintf(fil2,"#Parameters   :\n");
           for (i=0; i<=gd; i++)
            fprintf(fil2,"p%d=% .15lE\n",i,loesung[i]);
           fprintf(fil2,"#Fit function:\n");
           fprintf(fil2,"fit(x)=p0+p1*x");
           for (i=2; i<=gd; i++)
            fprintf(fil2,"+p%d*x**%d",i,i);
           fprintf(fil2,"\n");
           fprintf(fil2,"#\n#\n#\n");
           fprintf(fil2,"#plot it:\n");
           if (limited==1) 
            fprintf(fil2,"plot [%lf:%lf] '%s', fit(x)\n",x_min,x_max,com2);
           else
            fprintf(fil2,"plot '%s', fit(x)\n",com2);
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",cmdfile);

         fil2=fopen(resfile,"w");   /*Ergebnisfile schreiben*/
         if (fil2!=NULL)
          {
           fprintf(fil2,"#File         : %s\n",resfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : polynom, degree %d\n",gd);
           fprintf(fil2,"#Fit function : y=p0+p1*x");
           for (i=2; i<=gd; i++)
            fprintf(fil2,"+p%d*x**%d",i,i);
           fprintf(fil2,"\n"); 
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#\n");
           fprintf(fil2,"#Parameters   :\n");
           for (i=0; i<=gd; i++)
            fprintf(fil2,"              p%d=% .15lE\n",i,loesung[i]);
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",resfile);
        }
       ;
      }
     else   /*Newton Fit*/
      {
       success=newton(
          com3,             /*Fitstring*/
          x,                /*Wertefelder*/
          y,
          n_val,            /*Anzahl Werte*/
          &start,           /*Startvektor*/
          &result,          /*Loesungsvektor*/
          accuracy);        /*Genauigkeit*/
       if (success==0)
        {
         fil2=fopen(cmdfile,"w");   /*Kommandofile schreiben*/
         if (fil2!=NULL)
          {
           int i;
           char par[3];

           strcpy(par,"p0");
           fprintf(fil2,"#File         : %s\n",cmdfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : Newton\n");
           fprintf(fil2,"#Fit function : %s\n",com3);
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#\n#\n#\n#\n#\n");
           fprintf(fil2,"#Parameters:\n");
           for (i=0; i<10; i++)         /*Parameter auf File*/
            {
             par[1]='0'+i;
             if (isin(par,com3)==0)
              fprintf(fil2,"%s=% .15lE\n",par,result.p[i]);
            }
           fprintf(fil2,"#Fit function:\n");
           fprintf(fil2,"fit(x)=%s\n",com3);
           fprintf(fil2,"#\n#\n#\n");
           fprintf(fil2,"#plot it:\n");
           if (limited==1) 
            fprintf(fil2,"plot [%lf:%lf] '%s', fit(x)\n",x_min,x_max,com2);
           else
            fprintf(fil2,"plot '%s', fit(x)\n",com2);
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",cmdfile);

         fil2=fopen(resfile,"w");   /*Ergebnisfile schreiben*/
         if (fil2!=NULL)
          {
           int i;
           char par[3];

           strcpy(par,"p0");
           fprintf(fil2,"#File         : %s\n",resfile); /*Infos auf File*/
           fprintf(fil2,"#Created by   : GNUTIL 2.01\n");
           fprintf(fil2,"#Sourcefile   : %s\n",com2);
           fprintf(fil2,"#Fit kind     : Newton\n");
           fprintf(fil2,"#Fit function : %s\n",com3);
           if (limited==1) 
            fprintf(fil2,"#Fit range    : % .15lE to % .15lE\n",x_min,x_max);
           else
            fprintf(fil2,"#Fit range    : whole file\n");
           fprintf(fil2,"#Results      : \n");
           for (i=0; i<10; i++)         /*Parameter auf File*/
            {
             par[1]='0'+i;
             if (isin(par,com3)==0)
              fprintf(fil2,"               %s=% .15lE\n",par,result.p[i]);
            }
           fclose(fil2);
          }
         else printf("ERROR! Unable to open %s.\n",resfile);
        }
       else ;
      }
    }
   else printf("ERROR! Unable to open file %s.\n",com2);
  } /*ende fit*/
 else if ( (strcmp(com1,"exit")==0) || (strcmp(com1,"quit")==0) ) 
  ;                                         /*Programmende*/
 else err=1;

 if (err) printf ("unknown command.\n");    /*Unbekanntes Kommando*/
} /*Ende der Hauptschleife*/

printf("Bye.\n");
return 0;
}
