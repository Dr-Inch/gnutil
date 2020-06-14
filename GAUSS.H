#include <errno.h>
#include <math.h>

#define MAXGRAD 20 

typedef double vector[MAXGRAD];
typedef vector matrix [MAXGRAD];

int gauss(matrix aa, vector bb, int n, vector x);
/*Gauss-Algorithmus: aa:Matrix des GS, bb: rechte Seite*/
/*n: Grad des GS, x: Loesungsvektor*/
/*Ergebnis: 0 fuer alles ok*/
/*         -1 bei singulaerer Matrix*/
/*         -2 bei Rechenfehler*/

void zeige(matrix a, vector b, int grad);
/*zeigt Gleichungssystem an*/

void lies(matrix a, vector b, int grad);
/*liest Gleichungssystem ein*/
