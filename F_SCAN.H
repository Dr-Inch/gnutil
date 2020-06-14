#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>

#define LEN 255         /*Stringlaenge 255*/

struct parblock         /*Parameterblock*/
 {
  double x;
  double y;
  double z;
  double t;
  double p[10];
  int    errflag;
 };


struct f_node   /* Knoten des Funktionsbaums*/
{
 int operator;  /* Verknuepfung zwischen next1 u. next2 bei binaerem*/
                /* Operator, Verweis auf wert bei unaerem Operator */
 double wert;   /* numerischer Wert des Knotens (falls vorhanden) */
 struct f_node *next1;  /* Zeiger auf die naechsten beiden Eintraege*/
 struct f_node *next2;
};



void wipe_function(struct f_node *p);
/*Gibt den Speicher eines Funktionsbaumes wieder frei*/
/*NIE aufrufen, bevor die Funktion gescannt wurde!!! */

struct f_node *scan(char *s);
                    /*ersetzt gewisse Ausdruecke durch 'aliases',*/
                    /*scannt dann s und legt einen Funktionsbaum */
                    /*vom Typ f_node an. Falls s unsinnig ist, */
                    /*wird NULL zurueckgegeben*/

int setconst(char *name, double wert);
/* setzt eine Konstante im Globalen Konstantenblock */
/*  0:  Konstante gesetzt oder redefiniert*/
/* -1:  Konstantenblock voll */

void show_nodes(struct f_node *p);
/*Zeigt Funktionsbaum an*/

double eval(struct f_node *f, struct parblock *p);
/*Berechnet Wert des Funktionsbaums f mit Parameterblock p*/
/*dazu wird eval2 aufgerufen, das die eigentliche Arbeit macht.*/
/*Die Fehlercode-Rueckgabe in p.errflag: 0: alles ok, -1 : Fehler*/ 
/*wird mittels der globalen Variablen errno aus math.h */
/*bewerkstelligt (Der alte Wert wird gesichert)*/
/*im Fehlerfall wird als Funktionswert 0.0 zurueckgegeben*/

struct f_node *diff(struct f_node *f, char* p);
/*liefert pointer auf Ableitung, NULL bei Fehler*/
