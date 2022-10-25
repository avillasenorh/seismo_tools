#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************
* C program to calculate the mid-point between two lat-lon coordinates
* along the great circle that joins both.
* Optionally, if an argument (not starting with the "-" character)
*  is passed, it is assumed to be the input file.
* Optionally, if the -o option is used, the next argument is
*  used as the output file.
*
* Options:
*
* -o outfile     : writes output to outfile instead of stdout
* -V             : verbose (default runs silently)
*
*
* Usage: % midpoint [ infile ] [-o outfile ] [ ... ] [ -V ]
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

void gdelaz(float lon1, float lat1, float lon2, float lat2,
            float *delta, float *azim, int geocen);
void glatlon(float lon1, float lat1, float delta, float azim,
            float *lon2, float *lat2, int geocen);

int main(int argc, char *argv[])
{
	FILE	*fp,*fpout;
	char	infile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];

	char	line[MAXLINE],data[MAXLINE];

	float	lonc, latc, lone, late;
	float	lat, lon;
	float	azim, delta, delta2;
	int	nf;
	int	i;

	int	lin=FALSE;
	int	lout=FALSE;
	int	verbose=FALSE;

	/* decode flag options */

	for (i=1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
				case 'o': /* ouptput file */
					if (argv[i][2] != '\0') strcpy(outfile,&argv[i][2]);
					else strcpy(outfile,argv[++i]);
					lout=TRUE;
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

	/* read input file */
	i=0;
	while (fgets(line,MAXLINE,fp) != NULL) {
		nf=sscanf(line, "%f %f %f %f", &lonc, &latc, &lone, &late);
		gdelaz(lonc, latc, lone, late, &delta, &azim, 1);
		delta2 = delta/2.0;
		glatlon(lonc, latc, delta2, azim, &lon, &lat, 1);
		fprintf(fpout,"%f %f\n",lon, lat);
		i++;
	}

	if (verbose) fprintf(stderr,"Number of lines read in: %d\n",i);

	if (lin) fclose(fp);
	if (lout) fclose(fpout);

	return 0;
}
