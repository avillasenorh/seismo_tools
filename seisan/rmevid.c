#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************

* C template for programs that read from stdin and write to stdout.
* Optionally, if an argument (not starting with the "-" character)
*  is passed, it is assumed to be the input file.
* Optionally, if the -o option is used, the next argument is
*  used as the output file.
*
* Usage: template [infile] -Fvalue -Lvalue1/value2 [-o outfile] [-h] [-V]
*
* Arguments: infile
*
* Options:
*
*       -Fvalue                 reads one floating-point value
*       -Lvalue1/value2         reads two floating-point values
*       -o outfile              output plain-text file [default writes to stdout]
*       -h                      write this help [default=no]
*       -V                      verbose [default runs silently]
*
* Author:           Antonio Villasenor, CSIC
* Date:             4-JUNE-2012
* History:          added "usage" to earlier version; based on GMT-style options
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define MAXEVENTS 10000
#define EVLEN    15
#define MAXCARDS 1000
#define CARDLEN  81

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

int trim(char s[]);
int substr(char *s, char *t, int n, int m);
void usage(int options);

int main(int argc, char *argv[])
{
	FILE	*fp,*fpev,*fpout;
	char	infile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];
	char	evfile[FILENAME_MAX];

	char	line[MAXLINE];

	int	neq=0, nev=0, nrej=0, ncard=0;

	int	i, j, k;
	int	slen;
	int	*evmatch;

	char	**card;
	char	**evid;

	char	iev[EVLEN];

	int	select=TRUE;
	int	lin=FALSE;
	int	lev=FALSE;
	int	lout=FALSE;
	int	verbose=FALSE;

	/* decode flag options */

	if (argc <= 1) {
		usage(0);
		exit(1);
	}

	for (i=1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
				case 'o': /* ouptput file */
					if (argv[i][2] != '\0') strcpy(outfile,&argv[i][2]);
					else strcpy(outfile,argv[++i]);
					lout=TRUE;
					break;
				case 'I': /* index file with event ids to remove */
					if (argv[i][2] != '\0') strcpy(evfile,&argv[i][2]);
					else strcpy(evfile,argv[++i]);
					lev=TRUE;
					break;
				case 'h': /* write help message and exit */
					usage(1);
					exit(1);
					break;
				case 'V': /* verbose */
					verbose=TRUE;
					break;
				default:
					break;
			}
		} else {
			if ((fp=fopen(argv[i],"r")) == NULL) {
				fprintf(stderr,"ERROR: cannot open input file: %s\n",argv[i]);
				exit(1);
			} else {
				strcpy(infile,argv[i]);
				lin=TRUE;
			}
		}
	}

	/* if no input file passed, read from stdin */

	if (! lin) fp=stdin;

	/* if no event file passed, exit */

	if (! lev) {
		fprintf(stderr,"ERROR: event file not passed\n");
		exit(1);
	} else {
		if ((fpev=fopen(evfile, "r")) == NULL) {
			fprintf(stderr,"ERROR: cannot open event file: %s\n", evfile);
			exit(1);
		}
	}

	/* if no output file passed, write to stdout */

	if (! lout) fpout=stdout;
	else {
		if ((fpout=fopen(outfile, "w")) == NULL) {
			fprintf(stderr,"ERROR: cannot open output file: %s\n", outfile);
			exit(1);
		}
	}

	/* allocate space for event id array */
	evid = (char **)(malloc(MAXEVENTS*sizeof(char *)));
	evid[0]=(char *)malloc(MAXEVENTS*EVLEN*sizeof(char));
	for (i = 1; i < MAXEVENTS; i++) evid[i]=evid[i-1]+EVLEN;

	evmatch = (int *)malloc(MAXEVENTS*sizeof(int));
	for (i = 0; i < MAXEVENTS; i++) evmatch[i]=0;

	/* allocate space for phase card array */
	card = (char **)(malloc(MAXCARDS*sizeof(char *)));
	card[0]=(char *)malloc(MAXCARDS*CARDLEN*sizeof(char));
	for (i = 1; i < MAXCARDS; i++) card[i]=card[i-1]+CARDLEN;

	nev=0;
	while (fgets(line,MAXLINE,fpev) != NULL) {
		trim(line);
		strcpy(evid[nev++],line);
	}
	fclose(fpev);

	/* read input file */
	neq=0;
	nrej=0;
	while (fgets(line,MAXLINE,fp) != NULL) {
		trim(line);
		if (line[79] == '1') {
			ncard=0;
			select = TRUE;
			neq++;
		} else if (line[79] == 'I') {
			substr(iev,line,61,14);
			for (i = 0; i < nev; i++) {
				if (strcmp(iev,evid[i]) == 0) {
					if (evmatch[i] > 0) fprintf(stderr, "WARNING: %s already rejected %d time(s)\n",iev,evmatch[i]);
					nrej++;
					evmatch[i]++;
					select = FALSE;
					break;
				}
			}
		} else if (line[0] == '\0') {
			/* write event if selected */
			if (select) {
				for (j = 0; j < ncard; j++) {
					slen=strlen(card[j]);
					if (slen < 80) {
						for (k = slen ; k < 80; k++) card[j][k] = ' ';
						card[j][80] = '\0';
					}
					fprintf(fpout, "%s\n", card[j]);
				}
				fprintf(fpout, "                                                                                \n");
			} else {
				/*if (verbose) fprintf(stderr, "Skipping %s\n",iev);*/
			}
		}
		strcpy(card[ncard++],line);
	}

	if (verbose) {
		fprintf(stderr, "Number of earthquakes read in: %d\n", neq);
		fprintf(stderr, "Number of events ids in list : %d\n", nev);
		fprintf(stderr, "Number of events rejected    : %d\n", nrej);
	}

	for (i = 0; i < nev; i++) {
		if (evmatch[i] == 0) {
			fprintf(stderr, "WARNING: this event id not found: %s\n", evid[i]);
		} else if (evmatch[i] > 1) {
			fprintf(stderr, "WARNING: this event id found %d times: %s\n", evmatch[i], evid[i]);
		}
	}

	if (lin) fclose(fp);
	if (lout) fclose(fpout);

	return 0;
}

void usage(int options)
{
        fprintf(stderr,
        "usage: template [infile] -Fvalue -Lvalue1/value2 [-o outfile] [-h] [-V]\n");

        if (options) fprintf(stderr,"\n\
        infile                 input file [default reads from stdin]\n\
        \n\
        -Fvalue                 reads one floating-point value\n\
        -Lvalue1/value2         reads two floating-point values\n\
        -o outfile              output plain-text file [default writes to stdout]\n\
        -h                      write this help [default=no]\n\
        -V                      verbose [default runs silently]\n\
        \n");

}
