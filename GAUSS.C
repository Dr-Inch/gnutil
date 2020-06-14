#include <stdio.h>
#include "gauss.h"


void lies(matrix a, vector b, int grad)
{
 int zeile, spalte;
 
 printf("Matrix:\n\n");
 for (zeile=0; zeile<grad; zeile ++)
  {
    printf("Zeile %d:\n",zeile+1);
    for (spalte=0; spalte<grad; spalte++)
     {
      printf("a[%2d,%2d]=",zeile+1,spalte+1);
      scanf("%lf",&(a[zeile][spalte]));
     }
  }
 printf("\nrechte Seite:\n");
 for (zeile=0; zeile<grad; zeile++)
  {
    printf("b[%2d]=");
    scanf("%lf",&b[zeile]);
  }
}

void zeige(matrix a, vector b, int grad)
{
int zeile,spalte;

 for (zeile=0; zeile<grad; zeile ++)
  {
    for (spalte=0; spalte<grad; spalte++)
     {
      printf("%lE ",a[zeile][spalte]);
     }
    printf("  %lE\n",b[zeile]);
  }
 printf("\n");
}


/*Funktionen, die gauss braucht*/

static void findgreatest(matrix a, int n, int i, int p[2])
/*Suche groesstes Pivot in Matrix a von Rang n ab Spalte/Zeile i*/
/*Koordinaten des Pivots in p[]*/
{
int l,m,l0,m0;

    l0=i;
    m0=i;
    for (l=i; l<n; l++)
     {
      for (m=i; m<n; m++)
       {
        if (a[l][m]>a[l0][m0])
          {
           l0=l;
           m0=m;
          }
       }
     }
    p[0]=l0;
    p[1]=m0;
}

static void tauschezeile(matrix a, int n, int i1, int i2)
/*tausche Zeile i1 gegen Zeile i2 in Matrix a mit Rang n*/
{
 double help;
 int l;

 for (l=0; l<n; l++)
  {
   help=a[i1][l];
   a[i1][l]=a[i2][l];
   a[i2][l]=help;
  }
}

static void tauschespalte(matrix a, int n, int i1, int i2)
/*tausche Spalte i1 gegen Spalte i2 in Matrix a mit Rang n*/
{
 double help;
 int l;

 for (l=0; l<n; l++)
  {
   help=a[l][i1];
   a[l][i1]=a[l][i2];
   a[l][i2]=help;
  }
}





int gauss(matrix aa, vector bb, int n, vector x)
/*Gauss-Algorithmus: aa:Matrix des GS, bb: rechte Seite*/
/*n: Grad des GS, x: Loesungsvektor*/
/*Ergebnis: 0 fuer alles ok*/
/*         -1 bei singulaerer Matrix*/
/*         -2 bei Rechenfehler*/
{
 int errnobak,ok;   /*zur Fehlerbehandlung*/
 int woisix[MAXGRAD];
                    /*Permutationsvektor zum Spaltentausch*/ 
 matrix a;          /*Backup der Parameter*/
 vector b;
 int i,j,k;         /*Schleifenindices*/
                    /*i:Zeile, j:Spalte, k: Hilfsindex*/
 int pos[2];        /*Zeiger auf groesstes Element der Matrix*/
 double rhelp;      /*Hilfe zum Vertauschen von doubles*/
 int ihelp;         /*Hilfe zum Vertauschen von integers*/
 double pivot;      /*das Wichtigste am ganzen Programm*/
 double summe;      /*Hilfsvariable*/


 errnobak=errno;        /*sichere errno*/
 errno=0;
 ok=0;                  /*default: alles ok*/

 for (i=0; i<n; i++)    /*belege Vertauschungsvektor*/
    woisix[i]=i;

 for (i=0; i<n; i++)    /*sichere altes Gleichungssystem*/
  {
    for (j=0; j<n; j++)
        a[i][j]=aa[i][j];
    b[i]=bb[i];
  }

  for (i=0; i<n-1; i++)
   {
    /* ****Pivotisieren**** */
    findgreatest(a,n,i,pos);
    if (a[pos[0]][pos[1]]==0) return -1;
    tauschezeile(a,n,i,pos[0]);             /*Matrixzeile tauschen*/
    rhelp=b[i];                             /*rechte Seite tauschen*/
    b[i]=b[pos[0]];
    b[pos[0]]=rhelp;
    tauschespalte(a,n,i,pos[1]);            /*Matrixspalte tauschen*/
    ihelp=woisix[i];                        /*merken, was wohin ge-*/
    woisix[i]=woisix[pos[1]];               /*tauscht wurde*/
    woisix[pos[1]]=ihelp;
    /* ****Eliminieren**** */
    for (k=i+1; k<n; k++)
     {
      pivot=a[k][i]/a[i][i];
      b[k]-=pivot*b[i];
      a[k][i]=0;
      for (j=i+1; j<n; j++)
        a[k][j]=a[k][j]-pivot*a[i][j];
     }   
   }
 /* ****Ruecksubstituieren**** */
 if (a[n-1][n-1]==0) return -1;
 x[woisix[n-1]]=b[n-1]/a[n-1][n-1];
 for (i=n-2; i>=0; i--)
  {
   summe=0;
   for (j=n-1; j>=i+1; j--)
     summe+=a[i][j]*x[woisix[j]];
   x[woisix[i]]=(b[i]-summe)/a[i][i];
  }
 if (errno!=0) ok=-2;   /*Rechenfehler?*/
 errno=errnobak;        /*restore altes errno*/
 return ok;
}
