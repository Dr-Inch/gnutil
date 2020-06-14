#include "newton.h"

int isin(char *ps, char *fits)  
/*stellt fest, ob der string ps in mathematisch sinnvoller */
/*Weise in fits vorkommt (d.h. nicht etwa x in axt sondern *x */
/*Rueckgabe: 0 fuer Vorkommen, -1 sonst*/
{
   char *f;
   char c1, c2;
   int ok;
 
   ok=-1;
   f=strstr(fits,ps);
   if (f!=NULL) 
    {
     if ( (f-fits) >0 )
      {
       if ( (f-fits) < (strlen(fits)-2) )   /*Parameter in der Mitte*/
        {
         c1=fits[(f-fits-1)];       /*linker character*/
         c2=fits[(f-fits+2)];       /*rechter character*/
         if (
              ( 
                (c1=='+') || (c1=='-') ||
                (c1=='*') || (c1=='/') || (c1=='(') 
              )
           &&
              ( 
                (c2=='+') || (c2=='-') ||
                (c2=='*') || (c2=='/') || (c2==')') 
              )
            )
         ok=0;
        }
       else                         /*Parameter am Ende*/
        {
         c1=fits[(f-fits-1)];       /*linker character*/
         if ( (c1=='+') || (c1=='-') || (c1=='*') || (c1=='/') )
         ok=0;
        }
      }
     else                           /*Parameter am Anfang*/
      {
       c2=fits[(f-fits+2)];     /*rechter character*/
       if ( (c2=='+') || (c2=='-') || (c2=='*') || (c2=='/') )
         ok=0;
      }
    }
   return ok;     
}

int newton(char *fits,              /*Fitstring*/
          double *wx,               /*Wertefelder*/
          double *wy,
          int wertezahl,            /*Anzahl Werte*/
          struct parblock *start,   /*Startvektor*/
          struct parblock *x,       /*Loesungsvektor*/
          double accuracy)          /*Genauigkeit*/
/* loest grad(fitf)=0. Ergebnis: 0 fuer ok, -1 sonst */
{
 char deltas[LEN];          /*String fuer Fehlerfunktion*/
 struct f_node *delta;      /*Fehlerfunktion*/
 struct f_node *grad[10];   /*Gradient derselben*/
 struct f_node *fuma[10][10];   /*Funktionalmatrix desselben*/
 matrix fumar;              /*Funktionalmatrix an bestimmter Stelle*/
 vector gradr;              /*minus Gradient an fester Stelle*/
 int ok;
 double step;               /*Betrag eines Schrittes*/
 vector ksi;                /*Der Schritt vektoriell*/
 int i,j,k;                 /*Schleifenvariablen*/
 char diffs[3];             /*nach was ableiten?*/
 int count;                 /*wieviele Schritte?*/

 char ps[3];                /*Hilfsvariable*/
 int n_param;               /*Anzahl Fitparameter*/
 int woisp[10];             /*Ordnungsvektor*/
 struct parblock xn;        /*n-ter Iterationswert*/

 double maxstep;            /*max. Schrittweite*/ 
 double damp;               /*Daempfung*/

 xn.x=(*start).x;           /*uebertrage Startwert*/
 xn.y=(*start).y;
 xn.z=(*start).z;
 xn.t=(*start).t;
 for (i=0; i<10; i++) xn.p[i]=(*start).p[i];
 
 
 n_param=0;                 /*Automatisches Parameterfinden*/
 strcpy(ps,"p0");           /*gesucht wird p0..p9*/
 for (i=0; i<10; i++)
  {
   ps[1]='0'+i;
   if (isin(ps,fits)==0) woisp[n_param++]=i;
  }         /*Ende automatisches Parameterfinden*/

 printf("     Newton fit with %d parameters.\n",n_param);
 if (n_param==0) 
  {
   printf("     ERROR. No fit parameters (p0..p9) encountered.\n");
   return -2;
  }
 printf("     Press any key to abort.\n");

 ok=0;

 /*pfriemle Fehlerfunktionsstring zusammen*/
 deltas[0]='(';
 strcpy(deltas+1,fits);
 strcat(deltas,"-y)**2");

 strcpy(diffs,"p0");
 delta=scan(deltas);

 if (delta!=NULL)
    {
     for (i=0; i<n_param; i++)  /*bilde grad(delta)*/
      {
       diffs[1]='0'+woisp[i];   /*setze Differentiationsparameter */
       if (ok==0) grad[i]=diff(delta,diffs);    
           else grad[i]=NULL;
       if (grad[i]==NULL)   
           ok=-1;   /*Fehler*/
      }
     for (i=0; i<n_param; i++)  /*bilde Funktionalmatrix*/
      {
       for (j=0; j<n_param; j++)
        {
         diffs[1]='0'+woisp[j]; /*setze Differentiationsparameter */
         if (ok==0) fuma[i][j]=diff(grad[i],diffs); 
             else fuma[i][j]=NULL;
         if (fuma[i][j]==NULL)  
             ok=-1; /*Fehler*/
        }
      }
    }
 else ok=-1;

 if (ok==0) /*fitten, falls das alles hinhaut*/
  {     
   step=2.0*accuracy+1.0;
   count=1;
   while (step>accuracy)
   {

if (kbhit())    /*der Notausgang*/
 {
  fflush(stdin);
  ok=-3;
  break;
 }
    /*berechne aktuellen Gradienten und Funktionalmatrix*/
    for (i=0; i<n_param; i++)
     {
      gradr[i]=0.0;             /*leere Gradient*/
      for (j=0; j<n_param; j++)
          fumar[i][j]=0.0;      /*leere Matrix*/
     }
    for (k=0; k<wertezahl; k++) /*ueber alle Werte*/
     {
      xn.x=wx[k];
      xn.y=wy[k];
      for (i=0; i<n_param; i++) /*Zeilen*/
       {
        gradr[i]-=eval(grad[i],&xn);    /*es muss -grad stehen*/
        if (xn.errflag!=0) 
         {
          ok=-2;    /*Fehler*/
          goto schluss;
         }
        for (j=0; j<n_param; j++)   /*Spalten*/
         {
          fumar[i][j]+=eval(fuma[i][j],&xn);
          if (xn.errflag!=0) 
           {
            ok=-2;  /*Fehler*/
            goto schluss;
           }          
         }
       }
     }
    ok=gauss(fumar,gradr,n_param,ksi);  /*loese Gleichungssystem*/
    if (ok!=0) 
     {
      ok=-2;
      goto schluss;
     }
    step=0.0;                       /*gehe einen Schritt*/
    maxstep=0.0;
    if ((count%10)==0) printf("      Step #%d:\n",count);
    for (i=0; i<n_param; i++)       /*(step=Betrag des Schritts)*/
     {
      step+=ksi[i]*ksi[i];
      maxstep+=xn.p[woisp[i]]*xn.p[woisp[i]];
     }
    step=sqrt(step);
    maxstep=0.25*sqrt(maxstep);
    if ( fabs(maxstep) <= fabs(accuracy) ) maxstep=HUGE_VAL;
    if (step>maxstep)               /*nicht zu weit gehen*/
     {
      damp=maxstep/step;
      step=maxstep;
     }
    else damp=1.0;	
    for (i=0; i<n_param; i++)       /*(step=Betrag des Schritts)*/
     {
      xn.p[woisp[i]]+=ksi[i]*damp;
      if ((count%10)==0) printf("          p%d=%lE\n",woisp[i],xn.p[woisp[i]]);/*Zwischenwerte drucken*/
     }
    count++;    /*erhoehe Zaehler*/
   }    /*ende while*/
  } /*ende if ok*/


 schluss:

 wipe_function(delta);  /*Aufraeumen*/
 for (i=0; i<n_param; i++) 
  {
   wipe_function(grad[i]);
   for (j=0; j<n_param; j++)
       wipe_function(fuma[i][j]);
  }

 if (ok==0)
  {
   printf("     Konvergence.\n");
   for (i=0; i<n_param; i++)    /*Loesungsvektor uebertragen*/
    {
       (*x).p[woisp[i]]=xn.p[woisp[i]];
       printf("          p%d=%lE\n",woisp[i],xn.p[woisp[i]]);/*Ergebnis*/
    }
  }
 else 
  {
   for (i=0; i<n_param; i++)
       (*x).p[woisp[i]]=0.0;        /*gib nullen zurueck*/
   if (ok==-1) printf("     Bad fit string or differentiation error.\n");
   else if (ok==-2) printf("     Mathematical error.\n");
   else 
    {
     printf ("     User break. Never be sure what you get.\n");
     for (i=0; i<n_param; i++)  /*Loesungsvektor uebertragen*/
      {
       (*x).p[woisp[i]]=xn.p[woisp[i]];
       printf("          p%d=%lE\n",woisp[i],xn.p[woisp[i]]);/*Ergebnis*/
      }
     ok=0;
    }
  }

 return ok;
}
