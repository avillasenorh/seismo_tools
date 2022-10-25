#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************
* Program: azi_c
*
* Options:
*
* -o outfile     : writes output to outfile instead of stdout
*
* -:             : toggles between lon-lat (default) and lat-lon (for input)
* -S             : minimum/start distance (in degrees) to compute azimuth
* -E             : maximum/end distance (in degrees) to compute azimuth
* -V             : verbose (default runs silently)
*
*
* Usage: % azi_c [ infile ] [-o outfile ] [ -Sdmin ] [ -Edmax ] [-:] [ -V ]
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

#define MAXSEGMENT	2048

void gdelaz(float lon1, float lat1, float lon2, float lat2,
            float *delta, float *azim, int geocen);

int main(int argc, char *argv[])
{
	FILE	*fp,*fpout;
	char	infile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];

	char	line[MAXLINE], data[MAXLINE];

	float	*lat, *lon;
	float	x, y;
	float	delta, azim;

	int	i, j, k;
	int	nf, iseg;

	float	dmin = 0.;
	float	dmax = 180.;

	int	lin=FALSE;
	int	lout=FALSE;
	int	xy_toggle=FALSE;
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
				case ':': /* read lat-lon instead of lon-lat */
					xy_toggle=TRUE;
					break;
				case 'S': /* start: minimum distance */
					sscanf(&argv[i][2], "%f", &dmin);
					break;
				case 'E': /* end: maximum distance */
					sscanf(&argv[i][2], "%f", &dmax);
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

	lon = (float *)malloc(MAXSEGMENT*sizeof(float));
	lat = (float *)malloc(MAXSEGMENT*sizeof(float));

	/* read input file */
	i=0;
	iseg=0;
	while (fgets(line,MAXLINE,fp) != NULL) {
		if (line[0] == '>') {
			/* compute azimuths for this segment if iseg > 0 */
			if (iseg > 0) {
				for (j = 0; j < iseg-1; j++) {
					for (k = j+1; k < iseg; k++) {
						gdelaz(lon[j], lat[j], lon[k], lat[k], &delta, &azim, 1);
						if (delta >= dmin && delta <= dmax) {
							fprintf(fpout, "%7.2f %7.3f\n", azim, delta);
						}
					}
				}
			}
			iseg=0;
		} else {
			nf=sscanf(line, "%f %f", &x, &y);
			if (nf != 2) {
				fprintf(stderr, "ERROR reading line %d (ignored):\n%s\n",i+1,line);
			} else {
				if (xy_toggle) {
					lon[iseg]=y;
					lat[iseg]=x;
				} else {
					lon[iseg]=x;
					lat[iseg]=y;
				}
				iseg++;
			}
		}
		i++;
	}
	/* compute azimuths for last segment if iseg > 0 */
	if (iseg > 0) {
		for (j = 0; j < iseg-1; j++) {
			for (k = j+1; k < iseg; k++) {
				gdelaz(lon[j], lat[j], lon[k], lat[k], &delta, &azim, 1);
				if (delta >= dmin && delta <= dmax) {
					fprintf(fpout, "%7.2f %7.3f\n", azim, delta);
				}
			}
		}
	}

	if (verbose) fprintf(stderr,"Number of lines read in: %d\n",i);

	if (lin) fclose(fp);
	if (lout) fclose(fpout);

	return 0;
}
