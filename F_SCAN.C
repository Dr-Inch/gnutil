#include "f_scan.h"


#define MAXCONST    255 /*max. 255 Konstanten*/
#define CONSTSIG    10  /*10 Stellen fuer Konstantennamen*/

struct one_const        /*Konstantenstruktur */
 {
  char name[CONSTSIG];
  double wert;
 };


static struct one_const constblock[MAXCONST];   /*Konstantenblock*/
static int number_of_constants=0;               /*Anzahl Konstanten*/
static int differentiation_error;           /*Fehler beim Ableiten*/

/*Rechenzeichen, Symbole und Operatoren*/

/*binaere Operatoren*/
#define PLUS    '+'
#define MINUS   '~'
#define MAL     '*'
#define GETEILT '/'
#define HOCH    '^'

/*aliases (werden vor dem Scannen ersetzt)*/
/*HOCH2 wird durch HOCH ersetzt, um Verwechslung mit MAL */
/*auszuschliessen. PLUS2 und MINUS2 werden nur an den*/
/*Stellen ersetzt, wo sie keine Vorzeichen oder Exponentvorzeichen*/
/*von Fliesskommazahlen sind. Das binaere PLUS bleibt, das*/
/*binaere MINUS2 wird durch MINUS ersetzt. Das unaere Vorzeichen*/
/*MINUS2 wird zum unaeren NEG, das unaere PLUS2 entfaellt.*/

#define MINUS2  '-'
#define HOCH2   "**"

/*Klammern*/
#define BRA     '('
#define KET     ')'

/*unaere Operatoren (numeriert) und deren ASCII-Strings*/


/*GNUPLOT unaere Operatoren*/
#define ABS     "abs"
#define ACOS    "acos"
#define ASIN    "asin"  
#define ATAN    "atan"
#define CEIL    "ceil"
#define COS     "cos"   
#define COSH    "cosh"
#define EXP     "exp"
#define FLOOR   "floor" 
#define INT     "int"
#define LOG     "log"
#define LOG10   "log10" 
#define SGN     "sgn"
#define SIN     "sin"
#define SINH    "sinh"  
#define SQRT    "sqrt"
#define TAN     "tan"
#define TANH    "tanh"  

/*NICHT GNUPLOT unaere Operatoren*/

#define COT     "cot"
#define COTH    "coth"
#define ASINH   "asinh"
#define ACOSH   "acosh"
#define ATANH   "atanh"
#define ACOTH   "acoth"

#define NEG     "@"     /* Vorzeichen Minus */
#define NEGC    '@'

#define XVAL    "x"     /*Variablen und Parameter*/
#define YVAL    "y"
#define ZVAL    "z"
#define TVAL    "t"
#define P0VAL   "p0"
#define P1VAL   "p1"
#define P2VAL   "p2"
#define P3VAL   "p3"
#define P4VAL   "p4"
#define P5VAL   "p5"
#define P6VAL   "p6"
#define P7VAL   "p7"
#define P8VAL   "p8"
#define P9VAL   "p9"

#define PIPI    "pi"    /*Die zwei wichtigsten Konstanten*/
#define EULER   "e"


/*jetzt die Nummern der Operatoren (fuer Funktionsbaum) */
/*Nummern zwischen 1 und 70*/
/*GNUPLOT unaere Operatoren*/
#define OABS    1
#define OACOS   2
#define OASIN   3   
#define OATAN   4
#define OCEIL   5
#define OCOS    6   
#define OCOSH   7
#define OEXP    8
#define OFLOOR  9   
#define OINT    10
#define OLOG    11
#define OLOG10  12  
#define OSGN    13
#define OSIN    14
#define OSINH   15  
#define OSQRT   16
#define OTAN    17
#define OTANH   18  

/*NICHT GNUPLOT unaere Operatoren*/

#define OCOT    19
#define OCOTH   20
#define OASINH  21
#define OACOSH  22
#define OATANH  23
#define OACOTH  24

#define ONEG    25  /* Vorzeichen Minus */


/*Wert-Operatoren (Nummer von 50..70)*/

#define OXVAL   50
#define OYVAL   51
#define OZVAL   52
#define OTVAL   53
#define OP0VAL  54 
#define OP1VAL  55
#define OP2VAL  56 
#define OP3VAL  57 
#define OP4VAL  58 
#define OP5VAL  59 
#define OP6VAL  60 
#define OP7VAL  61 
#define OP8VAL  62 
#define OP9VAL  63 


#define OCONST  68  /*Konstanten-Operator*/
#define ONUM    69  /*Operator fuer numerischen Wert*/  
                    /*(diese zwei haben oben kein Gegenstueck)*/

/*binaere Operatoren (Nummer zwischen 70 und 100)*/

#define OPLUS   70
#define OMINUS  71
#define OMAL    72
#define OGETEILT 73
#define OHOCH   74

static char op_names[75][10] ={
                               "",ABS,ACOS,ASIN,ATAN,CEIL,COS,COSH,
                               EXP,FLOOR,INT,LOG,LOG10,SGN,SIN,SINH,
                               SQRT,TAN,TANH,COT,COTH,ASINH,ACOSH,
                               ATANH,ACOTH,"NEG",
/*24 mal nichts */             "","","","","","","","","","","","",
                               "","","","","","","","","","","","",
                               XVAL,YVAL,ZVAL,TVAL,P0VAL,P1VAL,P2VAL,
                               P3VAL,P4VAL,P5VAL,P6VAL,P7VAL,P8VAL,
                               P9VAL,
/*4 mal nichts*/               "","","","",
                               "CONST","NUM","+",
                               "-","*","/","**"
                              };



static int nbra(char *s)        /* Anzahl von BRA's in s */
{
 int i,l,count;
 
 count=0;
 l=strlen(s);
 for (i=0; i<l; i++)
    if (s[i]==BRA) count++;
 return count;
}

static int nket(char *s)        /* Anzahl von KETS in s */
{
 int i,l,count;
 
 count=0;
 l=strlen(s);
 for (i=0; i<l; i++)
    if (s[i]==KET) count++;
 return count;
}

static void noblanks(char *s)  /*Entfernt alle Blanks aus s*/
{
 int i,j,len;

 len=strlen(s);
 for (i=0; i<len; i++)
  {
   if (s[i]==' ') 
     {
      len--;
      for (j=i; j<len; j++) 
        s[j]=s[j+1];
      i--;
     }
  }
 s[len]=0;
}


static void replace(char *s, char *ss, char c)  
/*der String ss wird an allen Stellen durch den Character c ersetzt*/ 
{
 char *sss;
 char *dummy;
 int lss;

 lss=strlen(ss);
 while ( (sss=strstr(s,ss)) != NULL )
  {
   s[sss-s]=c;  /*ersetze String durch c*/
   dummy=strcpy(sss+1,sss+lss); /*lasse Rest nachrutschen*/
  }
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

static void makeproper(char *s) /*beseitigt alle Unklarheiten, auf */
                                /*die SCAN stossen koennte*/
{
 int i,l;

 noblanks(s);               /*loesche alle Blanks*/
 replace(s,HOCH2,HOCH);     /*Ersetze HOCH2 durch den eindeutigen */
                            /*Operator HOCH */


 if ( ( isdigit(s[1])==0 ) && ( s[0]==MINUS2 )
        && (s[1]!='.' ) ) s[0]=NEGC;
 /* Erstes Zeichen == Minus und KEINE Ziffer */
 /* oder Dezimalpunkt danach ==> NEGC */

 if (s[0]==PLUS) strdel(s,0,1);
 /* Erstes Zeichen == Plus ==> loeschen */


 l=strlen(s)-1;
 
 for (i=1; i<l; i++)
  {
       if ( (s[i]==PLUS) && (s[i-1]==BRA) ) strdel(s,i,1);
  else if (s[i]==MINUS2) 
       {
        if ( ((isdigit(s[i+1])!=0) || (s[i+1]=='.'))
           && (s[i-1]==BRA) );
        else if (s[i-1]==BRA)  s[i]=NEGC; 
        else if ( (s[i-1]=='E') || (s[i-1]=='e') ) ;
        else s[i]=MINUS;
       }
/*Alles ersetzen lt. Kommentar am Anfang */  
  }
}

static void rmbrackets(char *s) /*loescht unnoetige aeussere Klammern*/
{
 int i;     /*Schleifenzaehler*/
 int len;   /*Stringlaenge*/
 int dodel; /*0: nicht loeschen; 1: loeschen*/
 int cnt;   /*Zaehler fuer '(' (cnt++)  und ')' (cnt--) */
 
if (s[0]==BRA)
{
 
 dodel=1;
 
 while (dodel==1)
 {
  cnt=0;
  len=strlen(s);
 
  for (i=0; i<len; i++)
   { 
     if ( s[i] == BRA ) cnt++;
    else 
     if ( s[i] == KET ) cnt--;
    
     if ( (cnt==0) && (i != len-1) ) dodel=0;
     /* zaehle Klammern: wenn VOR dem Stringende einmal GLEICHVIELE */
     /* Klammern auf- und wieder zugegengen sind, loesche nicht.    */
   }
  if (dodel==1) /* loesche aeussere Klammern */
   {
    for (i=0; i<len-2; i++) /*rutsche nach links*/
     s[i]=s[i+1];
     s[i]=0;                    /*abschliessende Null*/
   }

/* printf("RMBRACKETS: s=%s\n",s); */

  if ( s[0] != BRA ) dodel=0;   /*hoere auf zu loeschen*/
 } /*Ende while dodel (alle unnoetigen aeusseren Klammern geloescht)*/
}  /*Ende if */
}  /*Ende rmbrackets*/


static int teile(char *s, char *s1, char *s2, int krit)
/*  teile teilt den String s in zwei wohlgeklammerte Ausdruecke */
/*  s1 und s2 in Abhaengigkeit vom Kriterium krit.              */
/*  ueberfluessige Klammern werden dabei eliminiert             */
/*  (z.B. (1+2)+3 ergibt  1+2 und 3)                            */
{
 char *ss;          /*Zeiger auf s*/
 char *tok;         /*Token-Zeiger*/
 int ok;            /*alles ok?*/
 char *dummy;       /*dummy-Rueckgabewert*/
 int len, i;        /*Stringlaenge und Schleifenvariable*/





if (krit==HOCH) 
{
 ss=s;              /*ss zeigt auf s*/ 
 ok=1;

/*Das ist links nach rechts, noetig bei HOCH*/
 while (ok==1)
  {
    tok=strchr(ss,krit);    /*krit nicht gefunden*/
    if (tok==NULL)
        ok=-1;
    else
       {
        s1=strncpy(s1,s, (size_t) (tok-s) );    /*teile String*/
        s1[tok-s]=0;                /*fuege Stringendezeichen an*/
        s2=strcpy(s2,s+(tok-s)+1);
        ss=tok+1;   /*scanne ab naechster Stelle*/
        /*Abbruch bei Aufteilung in 2 wohlgeklammerte Ausdruecke*/
        if ( (nbra(s1)==nket(s1)) && (nbra(s2)==nket(s2)) )
            ok=0;
       }
  }
/*Ende links nach rechts*/
}

/*Das ist rechts nach links, noetig bei allem sonst*/
else
{
 len=strlen(s);
 ok=-1;
 for (i=len-1; i>=0; i--)
  {
   if (s[i]==krit) 
    {
     s1=strncpy(s1,s,i);        /*Teile String auf*/
     s1[i]=0;
     s2=strcpy(s2,s+i+1);
     if ( (nbra(s1)==nket(s1)) && (nbra(s2)==nket(s2)) )
      {
        ok=0;
        rmbrackets(s1);
        rmbrackets(s2);
        break;
      }
    }
  }
}

/*ACHTUNG*/

/*
printf("Teile %s nach %c: ok=%d\n",s,krit,ok);
if (ok==0)
{
printf("s1=");
puts(s1); 
printf("s2=");
puts(s2);
printf("\n"); 
}
*/

 return ok; /* Rueckgabe: 0: alles o.k.  -1: Fehler */
}


static struct f_node *new_node(void)
/*Ergibt einen Zeiger auf f_node */
/*der benoetigte Speicher wird mit MALLOC reserviert*/
/*ist kein Speicher mehr frei, so erfolgt ein Programmabbruch*/
{
 struct f_node *p;

 p=(struct f_node *) malloc(sizeof(struct f_node));
 if (p==NULL) abort();
 (*p).next1=NULL;   /*vorbelegen*/
 (*p).next2=NULL;
 return p;
}


static int anfang(char *s1, char *s2)
/*ergibt 1, wenn s1 der Anfang von s2 ist und loescht dann diesen*/
/*Anfang aus s2 heraus. Ansonsten wird Null zurueckgeliefert*/
{
 int l, erg;

 l=strlen(s1);

/* printf("ANFANG: s1=%s s2=%s l=%d\n",s1,s2,l);  */

 erg=strncmp(s1,s2,l);
 if (erg) return 0;
 else 
  {
   strdel(s2,0,l); 
   return 1;
  }
}


static int scanun(char *s, struct f_node *node);
                        /* Prototyping fuer scanun */
                        /* (noetig wg. verschraenkt rekursivem */
                        /* Aufruf v. scanun u. scanbin) */

static int scanbin(char *s, struct f_node *node)
                    /* scannt einen String nach wohlgeklammerten */
{                   /* Ausdruecken, die durch binaere Operatoren */
                    /* getrennt sind.   */

 char s1[LEN], s2[LEN];     /* Teilstrings */
 int ok;
 struct f_node *next_node1, *next_node2;    /*naechste Knoten*/

 rmbrackets(s);             /*loesche unnoetige aeussere Klammern*/

/*printf("SCANBIN: s=%s\n",s); */

 ok=teile(s,s1,s2,PLUS);            /*Teile String nach PLUS */
 if (ok==0)
  {
    (*node).operator=OPLUS;
  }
 else
  {
  ok=teile(s,s1,s2,MINUS);          /*Teile String nach MINUS */
  if (ok==0)
   {
     (*node).operator=OMINUS;
   }
  else
   {
   ok=teile(s,s1,s2,MAL);           /*Teile String nach MAL */
   if (ok==0)
    {
      (*node).operator=OMAL;
    }
   else 
    {
    ok=teile(s,s1,s2,GETEILT);      /*Teile String nach GETEILT */
    if (ok==0)
     {
       (*node).operator=OGETEILT;
     }
    else
     {
     if (anfang(NEG,s)==1)  /*Ausnahme: HOCH bindet staerker als NEG*/
      {
        (*node).operator=ONEG;      
        ok=-2;  /*Verhindere rekursiven Aufruf von scanun, scanbin*/ 
      }
     else
      {
      ok=teile(s,s1,s2,HOCH);       /*Teile String nach HOCH */ 
      if (ok==0)
       {
         (*node).operator=OHOCH;
       }

      }     /*ende aller elses*/
     }
    }
   }
  }

 if (ok==0) 
  {
   next_node1=new_node();
   next_node2=new_node();
   (*node).next1=next_node1;    /*Verbinde Knoten*/
   (*node).next2=next_node2;
   ok=( scanbin(s1,next_node1) || scanbin(s2,next_node2) );
                                /*Rekursion*/
  }
 else if (ok==-2)   /*Ausnahme: NEG vor HOCH*/
  {
    next_node1=new_node();
    (*node).next1=next_node1;
    (*node).next2=NULL;
    ok=scanbin(s,next_node1);
  }
 else ok=scanun(s,node);        /*scanne nach unaeren Operatoren*/

 return ok;
}


static int scanun(char *s, struct f_node *node) 
/*scannt s nach unaeren Operatoren*/
{
 struct f_node *next_node;  /*Zeiger auf naechsten Knoten*/
 int ok;                    /*Hilfsvariable*/
 char *endp;                /*ebenso*/
 int i;                     /*Schleifenzaehler*/
 int len;                   /*Stringlaenge*/

/* printf("SCANUN: s=%s\n",s); */

 rmbrackets(s);         /*loesche ueberfluessige aeussere Klammern*/
 ok=-1;                 /*Flag=-1 fuer nichts gefunden*/
                        /*      1 fuer Operator gefunden, nach dem*/
                        /*         es nicht weitergeht, z.B. XVAL */
                        /*      0 fuer Operator mit weiterem Argument*/
                        /*        (z.B SIN) */

 if (anfang(ASINH,s))           /*diese Operatoren zuerst, da sie */
    {                           /*mit einem anderen Operator  */
     (*node).operator=OASINH;   /*anfangen */
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ACOSH,s)) 
    {
     (*node).operator=OACOSH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ATANH,s)) 
    {
     (*node).operator=OATANH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ACOTH,s)) 
    {
     (*node).operator=OACOTH;
     ok=0;  /*Es geht weiter*/
    } 
 else if (anfang(COSH,s))   /*cosh zuerst, da cosh mit cos anfaengt*/
    {
     (*node).operator=OCOSH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(LOG10,s))  /*log10 zuerst, da log10 mit log anfaengt*/
    {
     (*node).operator=OLOG10;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(SINH,s))   /*sinh zuerst, da es mit sin anfaengt*/
    {
     (*node).operator=OSINH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(TANH,s))   /*tanh zuerst, da es mit tan anfaengt*/
    {
     (*node).operator=OTANH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(COTH,s))   /*coth zuerst, da es mit cot anfaengt*/
    {
     (*node).operator=OCOTH;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ABS,s)) 
    {
     (*node).operator=OABS;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ACOS,s)) 
    {
     (*node).operator=OACOS;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ASIN,s)) 
    {
     (*node).operator=OASIN;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(ATAN,s)) 
    {
     (*node).operator=OATAN;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(CEIL,s)) 
    {
     (*node).operator=OCEIL;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(COS,s)) 
    {
     (*node).operator=OCOS;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(EXP,s)) 
    {
     (*node).operator=OEXP;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(FLOOR,s)) 
    {
     (*node).operator=OFLOOR;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(INT,s)) 
    {
     (*node).operator=OINT;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(LOG,s)) 
    {
     (*node).operator=OLOG;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(SGN,s)) 
    {
     (*node).operator=OSGN;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(SIN,s)) 
    {
     (*node).operator=OSIN;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(SQRT,s)) 
    {
     (*node).operator=OSQRT;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(TAN,s)) 
    {
     (*node).operator=OTAN;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(COT,s)) 
    {
     (*node).operator=OCOT;
     ok=0;  /*Es geht weiter*/
    }
 else if (anfang(NEG,s)) 
    {
     (*node).operator=ONEG;
     ok=0;  /*Es geht weiter*/
    }

 /*Jetzt Wert-Operatoren, Vergleich mit strcmp, da nach*/
 /*einem Wert-Operator nichts mehr kommen kann*/

 else if (strcmp(XVAL,s)==0) 
    {
     (*node).operator=OXVAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(YVAL,s)==0) 
    {
     (*node).operator=OYVAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(ZVAL,s)==0) 
    {
     (*node).operator=OZVAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(TVAL,s)==0) 
    {
     (*node).operator=OTVAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P0VAL,s)==0) 
    {
     (*node).operator=OP0VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P1VAL,s)==0) 
    {
     (*node).operator=OP1VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P2VAL,s)==0) 
    {
     (*node).operator=OP2VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P3VAL,s)==0) 
    {
     (*node).operator=OP3VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P4VAL,s)==0) 
    {
     (*node).operator=OP4VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P5VAL,s)==0) 
    {
     (*node).operator=OP5VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P6VAL,s)==0) 
    {
     (*node).operator=OP6VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P7VAL,s)==0) 
    {
     (*node).operator=OP7VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P8VAL,s)==0) 
    {
     (*node).operator=OP8VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
 else if (strcmp(P9VAL,s)==0) 
    {
     (*node).operator=OP9VAL;
     ok=1;  /*Es geht nicht weiter*/
    }
  
 if (ok==0)     /*Unaeren Operator gefunden, wo's weitergeht*/
  {
    next_node=new_node();
    (*node).next1=next_node;
    (*node).next2=NULL;
    ok=scanbin(s,next_node);    /*Rekursion*/
  }
 else           /*Ende des Baums erzeugen*/
  {
    (*node).next1=NULL;
    (*node).next2=NULL;
    if (ok==-1) /*nichts gefunden ==> pruefe auf Konstante*/
                /*falls da nichts ist, versuche auf double zu wandeln*/ 
      {
       for (i=0; i<number_of_constants; i++)
        {
         if (strcmp(constblock[i].name,s)==0)
            {
/*printf("Konstante %d von %d in %s gefunden\n",i,number_of_constants,s);*/
             ok=1;  /*Flag setzen*/
             (*node).operator=OCONST;
             (*node).next1=(struct f_node *) &constblock[i].wert;
             (*node).wert=i;
             break;
             /*trage einen Zeiger auf den Konstantenwert in den Baum*/
             /*(das ist zwar etwas unsauber, sorgt aber dafuer, dass*/
             /*selbst nach dem aendern einer Konstanten der richtige*/
             /*Wert berechnet werden kann)*/
             /*merke auch Konstantennummer*/
            }
        }
       if (ok==-1)  /*immer noch nichts gefunden? Wandle in double*/
        {
         (*node).operator=ONUM;
         if (strcmp(s,PIPI)==0) /*setze pi ein*/ 
           {
            ok=0;
            (*node).wert=M_PI;
           }
         else if (strcmp(s,EULER)==0)   /*setze e ein*/
           {
            ok=0;
            (*node).wert=M_E;
           }
         else
           {
            if ( (len=strlen(s)) >0 )
             {
              (*node).wert=strtod(s,&endp); /*wandle*/
              if ( ( (unsigned long) (endp-s) ) == len ) ok=0;
             }
           }
        }
      }
  }
 
 if (ok==1) ok=0;   /*1 ist auch in Ordnung*/
 return ok;
}


/*Ab jetzt Routinen, die von MAIN aufgerufen werden koennen*/




void wipe_function(struct f_node *p)
/*Gibt den Speicher eines Funktionsbaumes wieder frei*/
/*NIE aufrufen, bevor die Funktion gescannt wurde!!! */
{
 if (p!=NULL)
 {
 if (  ((*p).next1==NULL) && ((*p).next2==NULL) ) free(p);
 else
  {
   if ((*p).next1!=NULL) wipe_function((*p).next1);
   if ((*p).next2!=NULL) wipe_function((*p).next2);
  }
 }
}



static int tree_simplify2( /* struct f_node *n1 */
        struct f_node * *adr)
/* vereinfacht eimal den Baum t, wird von tree_simplify aufgerufen*/
/* (siehe dort) . Ergibt 0, falls etwas vereinfacht wurde, -1 sonst*/
{
 int ok;
 struct f_node *n1, *n2, *n3;

 ok=-1;
 n1=*adr;

 if (n1!=NULL)
   if ((*n1).operator!=OCONST)
                    /*Abbruch bei NULL-Zeiger oder Konstante*/
 {
  if ((*n1).operator==OMAL) /*Mal 1 oder mal 0 vereinfachen*/
   {
    n2=(*n1).next1;
    n3=(*n1).next2;
    if ((*n2).operator==ONUM)   /*links Zahl */
     {
      if ((*n2).wert==1.0)  /* 1*x=x */
       {
        *adr=n3;    
        free(n1);
        free(n2);
        ok=0;               /* Flag setzen */
       }
      else if ((*n2).wert==0.0) /* 0*x=0 */
       {
        (*n1).operator=ONUM;
        (*n1).next1=NULL;
        (*n1).next2=NULL;
        (*n1).wert=0.0;
        free(n2);
        wipe_function(n3);
        ok=0;               /* Flag setzen */
       }
     }  /* ende if ((*n2).operator==ONUM */
    if ( ((*n3).operator==ONUM) && (ok==-1) ) /*rechts Zahl*/
     {
      if ((*n3).wert==1.0)  /* x*1=x */
       {
        *adr=n2;    
        free(n1);
        free(n3);
        ok=0;               /* Flag setzen */
       }
      else if ((*n3).wert==0.0) /* 0*x=0 */
       {
        (*n1).operator=ONUM;
        (*n1).next1=NULL;
        (*n1).next2=NULL;
        (*n1).wert=0.0;
        free(n3);
        wipe_function(n2);
        ok=0;               /* Flag setzen */
       }
     }  /* ende if ((*n3).operator==ONUM) */
   }    /* ende if ((*n1).operator==OMAL) */

  else if ((*n1).operator==OHOCH)   /*hoch 1 oder hoch 0 vereinfachen*/
   {
    n2=(*n1).next1;
    n3=(*n1).next2;
    if ((*n2).operator==ONUM)   /*links Zahl */
     {
      if ((*n2).wert==1.0)  /* 1**x=1 */
       {
        (*n1).operator=ONUM;
        (*n1).next1=NULL;
        (*n1).next2=NULL;
        (*n1).wert=1.0;
        free(n2);
        wipe_function(n3);
        ok=0;               /* Flag setzen */
       }
     /* kein else, da 0**x=? */
     }  /* ende if ((*n2).operator==ONUM */
    if ( ((*n3).operator==ONUM) && (ok==-1) )   /*rechts Zahl*/
     {
      if ((*n3).wert==1.0)  /* x**1=x */
       {
        *adr=n2;    
        free(n1);
        free(n3);
        ok=0;               /* Flag setzen */
       }
      else if ((*n3).wert==0.0) /* x**0=1 */
       {
        (*n1).operator=ONUM;
        (*n1).next1=NULL;
        (*n1).next2=NULL;
        (*n1).wert=1.0;
        free(n3);
        wipe_function(n2);
        ok=0;               /* Flag setzen */
       }
     }  /* ende if ((*n3).operator==ONUM) */
   }    /* ende if ((*n1).operator==OHOCH) */

  else if ( ((*n1).operator==OPLUS) || ((*n1).operator==OMINUS) )
 /*plus oder minus 0 vereinfachen*/
   {
    n2=(*n1).next1;
    n3=(*n1).next2;
    if ((*n2).operator==ONUM)   /*links Zahl */
     {
      if ((*n2).wert==0.0)  /* 0+-x=+-x */
       {
        if ((*n1).operator==OPLUS)
         {
          *adr=n3;          /*0+x=x*/
          free(n1);
          free(n2);
          ok=0;             /* Flag setzen */
         }
        else 
         {                  /*0-x=-x*/
          (*n1).operator=ONEG;
          (*n1).next1=n3;
          (*n1).next2=NULL;
          free(n2); 
          ok=0;
         }
       }
     }  /* ende if ((*n2).operator==ONUM */
    if ( ((*n3).operator==ONUM) && (ok==-1) ) /*rechts Zahl*/
     {
      if ((*n3).wert==0.0)  /* x+-0=x */
       {
        *adr=n2;    
        free(n1);
        free(n3);
        ok=0;               /* Flag setzen */
       }
     }  /* ende if ((*n3).operator==ONUM) */
   }    /* ende if ((*n1).operator==OPLUS/MINUS) */


  /*Rekursion*/
  n1=*adr;
  if ( (*n1).next1!=NULL ) ok=( ok & tree_simplify2(&(*n1).next1) );
  if ( (*n1).next2!=NULL ) ok=( ok & tree_simplify2(&(*n1).next2) );
 }
 return ok;
}



static void tree_simplify(struct f_node * *t)
/* Unbedingt mit ADRESSOPERATOR aufrufen! */
/* Geht sonst in die Hose */
/* Vereinfacht den Baum t so, dass alle *0 und *1 sowie +-0 und **1 */
/* entfernt werden. Ruft dabei tree_simplify2 sooft auf, bis */
/* nichts mehr vereinfacht werden kann */
/* Kann von main nicht erreicht werden */
{
 while (tree_simplify2(t)==0) 
 ;
}






struct f_node *scan(char *s)
                    /*ersetzt gewisse Ausdruecke durch 'aliases',*/
                    /*scannt dann s und legt einen Funktionsbaum */
                    /*vom Typ f_node an. Falls s unsinnig ist, */
                    /*wird NULL zurueckgegeben*/
{
 int ok;
 char sss[LEN];     /*Speicher fuer Backup*/
 char *ss;          /*Lvalue dazu*/
 struct f_node *node;   /*Zeiger auf den 1.Knoten des Funktionsbaums*/
 
 ss=sss;
 ss=strcpy(ss,s);   /*Backup von s*/
 node=new_node();       /*Fordere Speicher fuer 1. Knoten an*/
 makeproper(ss);    /*Beseitige alle Unklarheiten*/

                            
 
 ok=scanbin(ss,node);
 if (ok!=0) 
  {
   wipe_function(node); 
   return NULL;
  }
 else
  {
   tree_simplify(&node);
   return node;             
  }
}

int setconst(char *name, double wert)
/* setzt eine Konstante im Globalen Konstantenblock */
/*  0:  Konstante gesetzt oder redefiniert*/
/* -1:  Konstantenblock voll */
{
 int i,found,ret;
 char *dummy;

 found=0;
 for (i=0; i<number_of_constants; i++)
  {
   if ( strcmp(name,constblock[i].name) == 0 ) 
   /*Vergleiche Namen mit allen bereits gesetzten Konstanten */
     {
     constblock[i].wert=wert;   /*Bei Uebereinstimmung Umsetzen */
     found=1;
     ret=0;
     break;
     }
  }
 if (found==0)  /*Falls nicht gesetzt, setze neu */
  {
   if (number_of_constants < MAXCONST)  /*Ueberlauf abfangen*/
    {
     dummy=strcpy(constblock[number_of_constants].name,name);
     constblock[number_of_constants].wert=wert;
     number_of_constants++;
     ret=0;
    }
   else 
    {
     ret=-1;
     printf("ERROR: Too many constants (max. %d)\n",MAXCONST);
    }
  }
 return ret;
}

void show_nodes(struct f_node *p)
/*Zeigt Funktionsbaum an*/
{
if (p!=NULL)
 {
 if ( ( ((*p).next1==NULL) && ((*p).next2==NULL) ) 
     || ((*p).operator==OCONST) )
  {
   printf("Operatornummer: %2d ; %s",(*p).operator,op_names[(*p).operator]);
   if ((*p).operator==ONUM) printf(" Wert: %lE\n",(*p).wert);
    else if ((*p).operator==OCONST) printf(" Name: %s\n",constblock[(int) ((*p).wert)].name); 
     else printf("\n");
  }
 else
  { 
   printf("Operatornummer: %2d ; %s\n",(*p).operator,op_names[(*p).operator]);
   if ((*p).next1!=NULL) 
      {
       show_nodes((*p).next1);
      }
   if ((*p).next2!=NULL) 
      {
       show_nodes((*p).next2);
      }
  }
 }
}

/*Kann nicht aus anderem Modul erreicht werden*/
static double eval2(struct f_node *f, struct parblock *p)
/*Berechnet Wert des Funktionsbaums f mit Parameterblock p*/
{
 int op;        /*Operator*/
 double erg;    /*Ergebnis der Berechnung*/
 
 op=(*f).operator;  /*hole Operator*/

 switch (op)
  {
   /*binaere Operatoren*/

   case OPLUS   :   erg=eval2((*f).next1,p)+eval2((*f).next2,p);
                    break;
   case OMINUS  :   erg=eval2((*f).next1,p)-eval2((*f).next2,p);
                    break;
   case OMAL    :   erg=eval2((*f).next1,p)*eval2((*f).next2,p);
                    break;
   case OGETEILT:   erg=eval2((*f).next1,p)/eval2((*f).next2,p);
                    break;
   case OHOCH   :   erg=pow( eval2((*f).next1,p),eval2((*f).next2,p) );
                    break;

   /*unaere Operatoren mit Fortsetzung*/

   case OABS    :   erg=fabs(eval2((*f).next1,p));
                    break;
   case OACOS   :   erg=acos(eval2((*f).next1,p));
                    break;
   case OASIN   :   erg=asin(eval2((*f).next1,p));
                    break;
   case OATAN   :   erg=atan(eval2((*f).next1,p));
                    break;
   case OCEIL   :   erg=ceil(eval2((*f).next1,p));
                    break;
   case OCOS    :   erg=cos(eval2((*f).next1,p));
                    break;
   case OCOSH   :   erg=cosh(eval2((*f).next1,p));
                    break;
   case OEXP    :   erg=exp(eval2((*f).next1,p));
                    break;
   case OFLOOR  :   erg=floor(eval2((*f).next1,p));
                    break;
   case OINT    :   erg=floor(eval2((*f).next1,p));
                    break;
   case OLOG    :   erg=log(eval2((*f).next1,p));
                    break;
   case OLOG10  :   erg=log10(eval2((*f).next1,p));
                    break;
   case OSGN    :   erg=eval2((*f).next1,p);
                    if (erg>0) erg=1.0;
                    else if (erg==0) erg=0.0;
                    else if (erg<0) erg=-1.0;
                    break;
   case OSIN    :   erg=sin(eval2((*f).next1,p));
                    break;
   case OSINH   :   erg=sinh(eval2((*f).next1,p));
                    break;
   case OSQRT   :   erg=sqrt(eval2((*f).next1,p));
                    break;
   case OTAN    :   erg=tan(eval2((*f).next1,p));
                    break;
   case OTANH   :   erg=tanh(eval2((*f).next1,p));
                    break;
   case OCOT    :   erg=1/tan(eval2((*f).next1,p));
                    break;
   case OCOTH   :   erg=1/tanh(eval2((*f).next1,p));
                    break;
   case OASINH  :   erg=asinh(eval2((*f).next1,p));
                    break;
   case OACOSH  :   erg=acosh(eval2((*f).next1,p));
                    break;
   case OATANH  :   erg=atanh(eval2((*f).next1,p));
                    break;
/*Vorsicht, atanh hat einen Bug: Bereichsueberschreitungen vergessen*/
   case OACOTH  :   erg=atanh(1/eval2((*f).next1,p));
                    break;
   case ONEG    :   erg=-(eval2((*f).next1,p));
                    break;

   /*unaere Operatoren ohne Fortsetzung*/

   case OXVAL   :   erg=(*p).x;
                    break;
   case OYVAL   :   erg=(*p).y;
                    break;
   case OZVAL   :   erg=(*p).z;
                    break;
   case OTVAL   :   erg=(*p).t;
                    break;
   case OP0VAL  :   erg=(*p).p[0];
                    break;
   case OP1VAL  :   erg=(*p).p[1];
                    break;
   case OP2VAL  :   erg=(*p).p[2];
                    break;
   case OP3VAL  :   erg=(*p).p[3];
                    break;
   case OP4VAL  :   erg=(*p).p[4];
                    break;
   case OP5VAL  :   erg=(*p).p[5];
                    break;
   case OP6VAL  :   erg=(*p).p[6];
                    break;
   case OP7VAL  :   erg=(*p).p[7];
                    break;
   case OP8VAL  :   erg=(*p).p[8];
                    break;
   case OP9VAL  :   erg=(*p).p[9];
                    break;
   case OCONST  :   erg=*( (double *) ((*f).next1) );
                    break;
   case ONUM    :   erg=(*f).wert;
                    break;

   default      :   erg=0;  /*Das sollte nie geschehen*/
                    printf("FEHLER: Fons hat einen Operator vergessen!\n");
                    errno=-1;
  }
 return erg;
}


double eval(struct f_node *f, struct parblock *p)
/*Berechnet Wert des Funktionsbaums f mit Parameterblock p*/
/*dazu wird eval2 aufgerufen, das die eigentliche Arbeit macht.*/
/*Die Fehlercode-Rueckgabe in p.errflag: 0: alles ok, -1 : Fehler*/ 
/*wird mittels der globalen Variablen errno aus math.h */
/*bewerkstelligt (Der alte Wert wird gesichert)*/
/*im Fehlerfall wird als Funktionswert 0.0 zurueckgegeben*/

{
 double erg;
 int errno_old;
 
 errno_old=errno;
 errno=0;
 if (f!=NULL) erg=eval2(f,p);   /*fange NULL-Pointer ab*/
 else errno=-1;
 if (errno!=0)
   {
    (*p).errflag=-1;
    erg=0.0;
   }
 else (*p).errflag=0;

 errno=errno_old;
 return erg;
}

/* kann nicht aus anderem Modul erreicht werden */
static struct f_node *tree_copy(struct f_node *p)
/*Kopiert einen Funktionsbaum ab der Stelle p*/
{
 struct f_node *neu;

 neu=new_node();
 (*neu)=(*p); 
 if ((*neu).operator==OCONST)
  ; /*Ende des Baums*/
 else
  {
  if ( (*p).next1!=NULL ) (*neu).next1=tree_copy((*p).next1);
  if ( (*p).next2!=NULL ) (*neu).next2=tree_copy((*p).next2);
  }
 return neu;
}



static struct f_node *diff2(struct f_node *f, char* p)
/* Wird von der Funktion diff aufgerufen, kann nicht von main*/
/* aufgerufen werden.*/
/* Differenziert den Baum f nach der Variablen p . Es wird */
/* ein Pointer auf die differenzierte Funktion zurueckgegeben*/
/* tritt ein Fehler auf, so wird die globale Variable */
/* differentiation_error auf den Wert -1 gesetzt*/
{
 struct f_node *ret;        /*Rueckgabe-Knoten*/
 int op;                    /*Operator des Knotens*/
 struct f_node *n1, *n2, *n3, *n4, *n5, *n6, *n7;   
 /*Weitere Knoten fuer komplexere Formeln*/

 op=(*f).operator;  /*hole Operator*/
 ret=new_node();    /*fordere neuen Knoten an*/
 (*ret).next1=NULL; /*setze defaultmaessig auf Fehler*/
 (*ret).next2=NULL;
 (*ret).operator=ONUM;

 switch (op)
  {
   /*binaere Operatoren*/

   case OPLUS   :   (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=diff2((*f).next2,p);
                    (*ret).operator=OPLUS;

                    break;

   case OMINUS  :   (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=diff2((*f).next2,p);
                    (*ret).operator=OMINUS;

                    break;

   case OMAL    :   n1=new_node();  /*Produktregel*/
                    n2=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=n2;
                    (*ret).operator=OPLUS;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=diff2((*f).next2,p);
                    (*n1).operator=OMAL;

                    (*n2).next1=tree_copy((*f).next2);
                    (*n2).next2=diff2((*f).next1,p);
                    (*n2).operator=OMAL;

                    break;

   case OGETEILT:   n1=new_node();  /* Quotientenregel */
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();
                    n5=new_node();

                    (*ret).next1=n2;
                    (*ret).next2=n1;
                    (*ret).operator=OGETEILT;

                    (*n1).next1=tree_copy((*f).next2);
                    (*n1).next2=n3;
                    (*n1).operator=OHOCH;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=2.0;

                    (*n2).next1=n4;
                    (*n2).next2=n5;
                    (*n2).operator=OMINUS;

                    (*n4).next1=tree_copy((*f).next2);
                    (*n4).next2=diff2((*f).next1,p);
                    (*n4).operator=OMAL;

                    (*n5).next1=tree_copy((*f).next1);
                    (*n5).next2=diff2((*f).next2,p);
                    (*n5).operator=OMAL;

                    break;

   case OHOCH   :   if ( (*((*f).next2)).operator == ONUM )
                      {
                        /*einfachster Fall: f(x)**Zahl*/
                        n1=new_node();
                        n2=new_node();
                        n3=new_node();
                        n4=new_node();
                        
                        (*ret).next1=n1;
                        (*ret).next2=n2;
                        (*ret).operator=OMAL;

                        (*n1).next1=NULL;
                        (*n1).next2=NULL;
                        (*n1).operator=ONUM;
                        (*n1).wert=(*((*f).next2)).wert;

                        (*n2).next1=n3;
                        (*n2).next2=diff2((*f).next1,p);
                        (*n2).operator=OMAL;

                        (*n3).next1=tree_copy((*f).next1);
                        (*n3).next2=n4;
                        (*n3).operator=OHOCH;

                        (*n4).next1=NULL;
                        (*n4).next2=NULL;
                        (*n4).operator=ONUM;
                        (*n4).wert=(*((*f).next2)).wert-1.0;
                      }
                    else if ( (*((*f).next2)).operator == OCONST )
                      {
                        /*zweiteinfachster Fall: f(x)**Konstante*/
                        n1=new_node();
                        n2=new_node();
                        n3=new_node();
                        n4=new_node();
                        n5=new_node();
                        n6=new_node();

                        (*ret).next1=n1;
                        (*ret).next2=n2;
                        (*ret).operator=OMAL;

                        (*n1).next1=(*((*f).next2)).next1;
                        (*n1).next2=NULL;
                        (*n1).operator=OCONST;

                        (*n2).next1=n3;
                        (*n2).next2=diff2((*f).next1,p);
                        (*n2).operator=OMAL;

                        (*n3).next1=tree_copy((*f).next1);
                        (*n3).next2=n4;
                        (*n3).operator=OHOCH;

                        (*n4).next1=n6;
                        (*n4).next2=n5;
                        (*n4).operator=OMINUS;

                        (*n5).next1=NULL;
                        (*n5).next2=NULL;
                        (*n5).operator=ONUM;
                        (*n5).wert=1.0;

                        (*n6).next1=(*((*f).next2)).next1;
                        (*n6).next2=NULL;
                        (*n6).operator=OCONST;
                      }
                    else
                      {
                        /*Allgemeinste Form fuer f(x)**g(x)*/
                        n1=new_node();
                        n2=new_node();
                        n3=new_node();
                        n4=new_node();
                        n5=new_node();
                    
                        (*ret).next1=tree_copy(f);
                        (*ret).next2=n1;
                        (*ret).operator=OMAL;
    
                        (*n1).next1=n2;
                        (*n1).next2=n4;
                        (*n1).operator=OPLUS;

                        (*n2).next1=diff2((*f).next2,p);
                        (*n2).next2=n3;
                        (*n2).operator=OMAL;

                        (*n3).next1=tree_copy((*f).next1);
                        (*n3).next2=NULL;
                        (*n3).operator=OLOG;
    
                        (*n4).next1=n5;
                        (*n4).next2=diff2((*f).next1,p);
                        (*n4).operator=OMAL;

                        (*n5).next1=tree_copy((*f).next2);
                        (*n5).next2=tree_copy((*f).next1);
                        (*n5).operator=OGETEILT;
                       }

                    break;

   /*unaere Operatoren mit Fortsetzung*/

   case OABS    :   n1=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=NULL;
                    (*n1).operator=OSGN;

                    break;

   case OACOS   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();
                    n5=new_node();
                    n6=new_node();
                    n7=new_node();
                    
                    (*ret).next1=n1;
                    (*ret).next2=NULL;
                    (*ret).operator=ONEG;

                    (*n1).next1=n2;
                    (*n1).next2=diff2((*f).next1,p);
                    (*n1).operator=OMAL;

                    (*n2).next1=n3;
                    (*n2).next2=n7;
                    (*n2).operator=OHOCH;

                    (*n3).next1=n4;
                    (*n3).next2=n5;
                    (*n3).operator=OMINUS;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=1.0;

                    (*n5).next1=tree_copy((*f).next1);
                    (*n5).next2=n6;
                    (*n5).operator=OHOCH;

                    (*n6).next1=NULL;
                    (*n6).next2=NULL;
                    (*n6).operator=ONUM;
                    (*n6).wert=2.0;

                    (*n7).next1=NULL;
                    (*n7).next2=NULL;
                    (*n7).operator=ONUM;
                    (*n7).wert=-0.5;

                    break;

   case OASIN   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();
                    n5=new_node();
                    n6=new_node();
                    
                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n4;
                    (*n1).operator=OHOCH;

                    (*n2).next1=n3;
                    (*n2).next2=n5;
                    (*n2).operator=OMINUS;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=1.0;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-0.5;

                    (*n5).next1=tree_copy((*f).next1);
                    (*n5).next2=n6;
                    (*n5).operator=OHOCH;

                    (*n6).next1=NULL;
                    (*n6).next2=NULL;
                    (*n6).operator=ONUM;
                    (*n6).wert=2.0;

                    break;

   case OATAN   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=n1;
                    (*ret).operator=OGETEILT;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OPLUS;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=1.0;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=n4;
                    (*n3).operator=OHOCH;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=2.0;

                    break;

   case OCEIL   :   /*Fehler*/
                    differentiation_error=-1;
                    break;

   case OCOS    :   n1=new_node();
                    n2=new_node();
        
                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=NULL;
                    (*n1).operator=ONEG;

                    (*n2).next1=tree_copy((*f).next1);
                    (*n2).next2=NULL;
                    (*n2).operator=OSIN;

                    break;

   case OCOSH   :   n1=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=NULL;
                    (*n1).operator=OSINH;

                    break;

   case OEXP    :   n1=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=NULL;
                    (*n1).operator=OEXP;

                    break;

   case OFLOOR  :   /*Fehler*/
                    differentiation_error=-1;
                    break;

   case OINT    :   /*Fehler*/
                    differentiation_error=-1;
                    break;

   case OLOG    :   n1=new_node();
                    n2=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=tree_copy((*f).next1);
                    (*n1).operator=OGETEILT;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=1.0;

                    break;

   case OLOG10  :   n1=new_node();
                    n2=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=tree_copy((*f).next1);
                    (*n1).operator=OGETEILT;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=1.0/log(10.0);

                    break;

   case OSGN    :   /*Fehler*/
                    differentiation_error=-1;
                    break;

   case OSIN    :   n1=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=NULL;
                    (*n1).operator=OCOS;

                    break;

   case OSINH   :   n1=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=tree_copy((*f).next1);
                    (*n1).next2=NULL;
                    (*n1).operator=OCOSH;

                    break;

   case OSQRT   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OMAL;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=0.5;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=n4;
                    (*n3).operator=OHOCH;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-0.5;

                    break;

   case OTAN    :   n1=new_node();
                    n2=new_node();
                    n3=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OHOCH;

                    (*n2).next1=tree_copy((*f).next1);
                    (*n2).next2=NULL;
                    (*n2).operator=OCOS;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=-2.0;

                    break;

   case OTANH   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OHOCH;

                    (*n2).next1=tree_copy((*f).next1);
                    (*n2).next2=NULL;
                    (*n2).operator=OCOSH;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=-2.0;

                    break;

   case OCOT    :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=NULL;
                    (*n1).operator=ONEG;

                    (*n2).next1=n3;
                    (*n2).next2=n4;
                    (*n2).operator=OHOCH;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=NULL;
                    (*n3).operator=OSIN;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-2.0;

                    break;

   case OCOTH   :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=NULL;
                    (*n1).operator=ONEG;

                    (*n2).next1=n3;
                    (*n2).next2=n4;
                    (*n2).operator=OHOCH;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=NULL;
                    (*n3).operator=OSINH;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-2.0;

                    break;

   case OASINH  :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();
                    n5=new_node();
                    n6=new_node();
                    
                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n4;
                    (*n1).operator=OHOCH;

                    (*n2).next1=n3;
                    (*n2).next2=n5;
                    (*n2).operator=OPLUS;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=1.0;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-0.5;

                    (*n5).next1=tree_copy((*f).next1);
                    (*n5).next2=n6;
                    (*n5).operator=OHOCH;

                    (*n6).next1=NULL;
                    (*n6).next2=NULL;
                    (*n6).operator=ONUM;
                    (*n6).wert=2.0;

                    break;

   case OACOSH  :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();
                    n5=new_node();
                    n6=new_node();
                    
                    (*ret).next1=n1;
                    (*ret).next2=diff2((*f).next1,p);
                    (*ret).operator=OMAL;

                    (*n1).next1=n2;
                    (*n1).next2=n4;
                    (*n1).operator=OHOCH;

                    (*n2).next1=n5;
                    (*n2).next2=n3;
                    (*n2).operator=OMINUS;

                    (*n3).next1=NULL;
                    (*n3).next2=NULL;
                    (*n3).operator=ONUM;
                    (*n3).wert=1.0;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=-0.5;

                    (*n5).next1=tree_copy((*f).next1);
                    (*n5).next2=n6;
                    (*n5).operator=OHOCH;

                    (*n6).next1=NULL;
                    (*n6).next2=NULL;
                    (*n6).operator=ONUM;
                    (*n6).wert=2.0;

                    break;

   case OATANH  :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=n1;
                    (*ret).operator=OGETEILT;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OMINUS;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=1.0;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=n4;
                    (*n3).operator=OHOCH;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=2.0;

                    break;

   case OACOTH  :   n1=new_node();
                    n2=new_node();
                    n3=new_node();
                    n4=new_node();

                    (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=n1;
                    (*ret).operator=OGETEILT;

                    (*n1).next1=n2;
                    (*n1).next2=n3;
                    (*n1).operator=OMINUS;

                    (*n2).next1=NULL;
                    (*n2).next2=NULL;
                    (*n2).operator=ONUM;
                    (*n2).wert=1.0;

                    (*n3).next1=tree_copy((*f).next1);
                    (*n3).next2=n4;
                    (*n3).operator=OHOCH;

                    (*n4).next1=NULL;
                    (*n4).next2=NULL;
                    (*n4).operator=ONUM;
                    (*n4).wert=2.0;

                    break;

   case ONEG    :   (*ret).next1=diff2((*f).next1,p);
                    (*ret).next2=NULL;
                    (*ret).operator=ONEG;

                    break;

   /*unaere Operatoren ohne Fortsetzung*/

   case OXVAL   :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,XVAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OYVAL   :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,YVAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OZVAL   :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,ZVAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OTVAL   :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,TVAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP0VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P0VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP1VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P1VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP2VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P2VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP3VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P3VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP4VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P4VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP5VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P5VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP6VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P6VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP7VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P7VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP8VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P8VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OP9VAL  :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    if (strcmp(p,P9VAL)==0)
                        (*ret).wert=1.0;
                    else
                        (*ret).wert=0.0;
                    break;

   case OCONST  :   {
                    int i;  /*Schleifenzaehler zum Vergleich mit*/
                            /*allen gesetzten Konstanten */

                    (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    (*ret).wert=0.0;    /*default*/
                    for (i=0; i<number_of_constants; i++)
                    /*Vergleiche p mit allen Konstantennamen*/
                     {
                      if (strcmp(p,constblock[i].name)==0)
                        {
                         (*ret).wert=1.0;
                         break;
                        }
                     }
                    }
                    break;

   case ONUM    :   (*ret).next1=NULL;
                    (*ret).next2=NULL;
                    (*ret).operator=ONUM;
                    (*ret).wert=0.0;
                    break;

   default      :   differentiation_error=-1;
                    ;   /*Das sollte nie geschehen*/
                    printf("FEHLER: Differentiation ");
                    printf("eines nichtexistenten Operators!\n");
  } 
 return(ret);
}   /* Ende diff2 */

struct f_node *diff(struct f_node *f, char* p)
{
 struct f_node *ret; 

 if (f!=NULL)   /*ist f ueberhaupt definiert?*/
  {
   differentiation_error=0; 
   ret=diff2(f,p);
   if (differentiation_error==-1) /*Gib NULL im Fehlerfall zurueck*/
    {                               /*und raeume den Baum auf */
     wipe_function(ret);
     ret=NULL;
    }
   else 
    {
     tree_simplify(&ret);   /*sonst:Vereinfache*/ 
/*elch
printf("diff nach vereinfachen\n");
show_nodes(ret);*/
    }
  }
 else ret=NULL;
 
 return ret;
}

