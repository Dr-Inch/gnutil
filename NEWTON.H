#include <stdio.h>
#include <string.h>
#include <math.h>
#include "special.h" /*fuer kbhit*/
#include "f_scan.h" 
#include "gauss.h"

int isin(char *ps, char *fits);
/*stellt fest, ob der string ps in mathematisch sinnvoller */
/*Weise in fits vorkommt (d.h. nicht etwa x in axt sondern *x */
/*Rueckgabe: 0 fuer Vorkommen, -1 sonst*/

int newton(char *fits,              /*Fitstring*/
          double *wx,               /*Wertefelder*/
          double *wy,
          int wertezahl,            /*Anzahl Werte*/
          struct parblock *start,   /*Startvektor*/
          struct parblock *x,       /*Loesungsvektor*/
          double accuracy);         /*Genauigkeit*/
/* loest grad(fitf)=0. Ergebnis: 0 fuer ok, -1 sonst */
