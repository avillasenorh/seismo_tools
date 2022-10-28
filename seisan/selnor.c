#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

	char	line[MAXLINE];
	char	aux[MAXLINE];

	int	neq=0, nsel=0, ncard=0;

	float	lon1 = -180.0, lon2 = 180.0, lat1 = -90.0 , lat2 = 90.0;
	float	depth1 = 0.0, depth2 = 700.0;
	float	mag1 = -9.9, mag2 = 9.9;

	int	year, month, day, hour, minute;
	float	second;
	float	lat, lon, depth;
	float	mag, mmag1, mmag2;

	int	i, j, k;
	int	nf, slen;

	char	**card;

	int	select=TRUE;
	int	lreg=FALSE;
	int	ldep=FALSE;
	int	lmag=FALSE;
	int	lin=FALSE;
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
				case 'R': /* read region */
					nf = sscanf(&argv[i][2], "%f/%f/%f/%f", &lon1, &lon2, &lat1, &lat2);
					if (nf != 4) {
						fprintf(stderr, "ERROR: invalid number of parameters for -R option: %d\n", nf);
						exit(1);
					}
					lreg = TRUE;
					break;
				case 'Z': /* read depth range */
					nf = sscanf(&argv[i][2], "%f/%f", &depth1, &depth2);
					if (nf != 2) {
						fprintf(stderr, "ERROR: invalid number of parameters for -Z option: %d\n", nf);
						exit(1);
					}
					ldep = TRUE;
					break;
				case 'M': /* read magnitude range */
					nf = sscanf(&argv[i][2], "%f/%f", &mag1, &mag2);
					if (nf != 2) {
						fprintf(stderr, "ERROR: invalid number of parameters for -M option: %d\n", nf);
						exit(1);
					}
					lmag = TRUE;
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

	/* if no output file passed, write to stdout */

	if (! lout) fpout=stdout;
	else {
		if ((fpout=fopen(outfile, "w")) == NULL) {
			fprintf(stderr,"ERROR: cannot open output file: %s\n", outfile);
			exit(1);
		}
	}

	/* allocate space for phase card array */
	card = (char **)(malloc(MAXCARDS*sizeof(char *)));
	card[0]=(char *)malloc(MAXCARDS*CARDLEN*sizeof(char));
	for (i = 1; i < MAXCARDS; i++) card[i]=card[i-1]+CARDLEN;

	/* read input file */
	neq=0;
	nsel=0;
	while (fgets(line,MAXLINE,fp) != NULL) {
		trim(line);
		if (line[79] == '1') {
			ncard=0;
			select = TRUE;
			substr(aux, line, 24, 7);	
			sscanf(aux, "%f", &lat);
			substr(aux, line, 31, 8);	
			sscanf(aux, "%f", &lon);
			substr(aux, line, 39, 5);	
			sscanf(aux, "%f", &depth);

			mmag1 = mmag2 = 0.0;
			if (line[57] == '.') {
				substr(aux, line, 57, 3);	
				sscanf(aux, "%f", &mmag1);
			}
			if (line[65] == '.') {
				substr(aux, line, 65, 3);	
				sscanf(aux, "%f", &mmag2);
			}
			mag = fmaxf(mmag1, mmag2);

			if (lreg && (lat < lat1 || lat > lat2 || lon < lon1 || lon > lon2)) select = FALSE;
			if (ldep && (depth < depth1 || depth > depth )) select = FALSE;
			if (lmag && (mag < mag1 || mag > mag2 )) select = FALSE;
			neq++;
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
		fprintf(stderr, "Number of events extracted   : %d\n", nsel);
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
