/*  Last edited: Jul 19 14:03 1996 (esr) */

/* mkswissPfam.c
 *
 *
 * Created by Erik Sonnhammer, 940902
 */
  
#define NAMESIZE 40
#define ACSIZE 40
#define MAXLINE     10000

typedef struct _MSP
{
    struct _MSP *next;
    char     qname[NAMESIZE+1];
    char     qAC[ACSIZE+1];
    int      qstart; 
    int      qend;
    char     sname[NAMESIZE+1];
    char    *sdesc;
    int      ssize;
} MSP;

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>

char  *drawstr=0, desc[MAXLINE];
int    resPerChar = 0, qlen, newQuery=1, namelen=40;
FILE  *file;


void *myalloc (int len)
{
  void* retval = malloc(len) ;

  if (!retval)
    { fprintf (stderr, "Memory request for %d bytes failed - ABORTING\n", len) ;
      exit (-1) ;
    }
  return retval ;
}

#define myfree(x) ((x) ? free(x), (x) = 0 : (x))

void fatal(char *format, ...)
{
    va_list  ap;

    printf("\nFATAL ERROR: "); 

    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);

    printf("\n"); 
    exit(-1);
}


/* DRAWLINE draws a symbolic subsequence on the screen
   separating MSP's and displaying small ones by 'maximizing the alternation'
*/
void drawline(char *str)
{
    int i, j;
    char c;
    
    c = ' ';
    for (i=1; i <= qlen; i += resPerChar) 
    {
	for (j=i; j <= qlen && j < i+resPerChar; j++) 
	    if (str[j] != c) { /* Check for alternate c */
		c = str[j];
		break;
	    }
	printf("%c", c);
    }
}


void printStructure(MSP *MSPlist)
{
    MSP   *msp;
    int    n=0, size;
    char  *name;
    static char coord[50], coords[10000];

    *coords = 0;

    memset(drawstr+1, ' ', qlen);
    for (msp = MSPlist->next; msp ; msp = msp->next)
    {
	name = msp->sname;
	size = msp->ssize;
	sprintf(coord, " %d-%d", msp->qstart, msp->qend);
	strcat(coords, coord);
	memset(drawstr + msp->qstart, '_', msp->qend - msp->qstart + 1);
	n++;
    }

    printf("%-*s %2d ", namelen, name, n);
    drawline(drawstr);
    printf(" (%d) %s %s\n", size, desc, coords);
    memset(drawstr+1, ' ', qlen);
}


static void mspcpy(MSP *dest, MSP *src)
{
    strcpy(dest->qname, src->qname);
    strcpy(dest->qAC, src->qAC);
    dest->qstart     = src->qstart; 
    dest->qend       = src->qend;
    strcpy(dest->sname, src->sname);
    dest->ssize      = src->ssize;
}


/* sortMSPs sorts the MSPs according to start position in Subject (a bit arbitrary)
*/
static void sortMSPs(MSP *MSPlist)
{
    MSP tmpmsp, *msp1, *msp2;

    if (!MSPlist->next) return;
    
    for (msp1 = MSPlist->next ; msp1->next ; msp1 = msp1->next )
    {
	for (msp2 = msp1->next ; msp2 ; msp2 = msp2->next )
	{
	    if ( msp1->qstart > msp2->qstart)
	    {
		mspcpy(&tmpmsp, msp2);
		mspcpy(msp2, msp1);
		mspcpy(msp1, &tmpmsp);
	    }
	}
    }
}


/* Unfortunately we can't use this, since every subject is printed as they are read and we
 * can't tell if a subject with a longer name is coming after... SHIT! */
static void maxNamelen(MSP *MSPlist)
{
    MSP *msp;
    int len;

    if (!MSPlist->next) return;
    
    for (namelen=0, msp = MSPlist->next ; msp->next ; msp = msp->next )
    {
	if (len = strlen(msp->sname) > namelen) namelen = len;
    }
}


char *nextWord(char **c)
{
    char *cp = *c;

    for (; *cp; cp++) if (*cp == ' ' || !*cp) break;
    for (; *cp; cp++) if (*cp != ' ') break;

    *c = cp;
    return cp;
}


/* GETNEXTMSPs reads all MSP's from one Database Subject into the list MSPlist
   Note that the input file has to be sorted on Query and secondly on Subject !!!
*/
void getnextMSPs(MSP *MSPlist)
{
    MSP   *msp;
    static char  *cp, line[MAXLINE+1], junk[MAXLINE+1], 
                 nextqname[NAMESIZE+1], nextsname[NAMESIZE+1], prevqname[NAMESIZE+1];
    int i;

    msp = MSPlist;

    if(!fgets(line, MAXLINE, file)) return;
    while (!feof(file))
    {
	sscanf(line, "%s%s%s%s%s%s", 
				 	       junk, nextqname, junk, junk, junk, nextsname);

	if (msp != MSPlist && 
				    ( strcmp(nextqname, msp->qname) || strcmp(nextsname, msp->sname) )) {
	    /* Put this line back and return*/
	    fseek(file, -strlen(line), SEEK_CUR);
	    strcpy(prevqname, msp->qname);
	    return;
	}

	if (strcmp(nextqname, prevqname)) newQuery = 1;

	msp->next = (MSP *)myalloc(sizeof(MSP));
	msp = msp->next;
	msp->next = 0;

	/*printf("%s\n", line);*/
	
	sscanf(line, "%d%s%s%d%d%s%d", 
				 	       &qlen, msp->qname, msp->qAC, &msp->qstart, &msp->qend, msp->sname, &msp->ssize);

	cp = line;
	for (i = 0; i < 7; i++) nextWord(&cp);
	strcpy(desc, cp);
	if (cp = strchr(desc, '\n')) *cp = 0;
	
	if(!fgets(line, MAXLINE, file)) break;
    }
}


/* ############ MAIN ############## */
void main(int argc, char *argv[])
{
    MSP    MSPlist, *msp, *prevmsp;
    int    first = 1;
    char  *usage = "\
mkswissPfam - create graphical representation of swissprot domain organization\n\
\n\
Usage:  mkswissPfam <data-file>\n\
\n\
the data-file should contain \"qlength, qname, qAC, qstart, qend, sname, ssize(members), [Description]\"\n\n";

    

    if (argc < 2) fatal("Bad parameters\n\n%s", usage);
	

    if (!(file = fopen( argv[argc-1], "r" )))
	fatal("Can't open file %s\n", argv[argc-1]);

    while ( !feof(file) )
    {
	getnextMSPs(&MSPlist); /* Get all HSP's for next Subject */
	sortMSPs(&MSPlist);
	/* maxNamelen(&MSPlist); */

	if (newQuery) {
	    /*if (!first) {
		printf("  Protein    Start - End    Domain Family  Family Members\n");
		printf(" ----------  -----   ----   -------------  --------------\n");
		printf(" %-10s  %5d - %-5d  %-6d         %-6d\n");
	    }*/

	    printf("\n>%-*s |", namelen+1, MSPlist.next->qname);
	    resPerChar = (int)qlen/50 + 1;
	
	    myfree(drawstr);
	    drawstr = (char *)myalloc(qlen+1);
    
	    memset(drawstr, '=', qlen+1);
	    drawline(drawstr); 
	    printf("| %s %d a.a.\n", MSPlist.next->qAC, qlen);

	    newQuery = 0;
	}

	printStructure(&MSPlist);

	/* Done with this Subject - free memory etc. 
	 ********************************************/

	for (msp = MSPlist.next ; msp ;)
	{
	    prevmsp = msp;
	    msp = msp->next;
	    myfree(prevmsp);
	}
	MSPlist.next = NULL;
    }

    fclose(file);
}
