#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************
* Program: da2geo 
*
* Options:
*
* -o outfile     : writes output to outfile instead of stdout
*
* -:             : toggles between lon-lat (default) and lat-lon (for input)
* -C             : uses geocentric coordinates (default)
* -G             : uses geographic coordinates (spherical Earth)
* -V             : verbose (default runs silently)
*
* Usage: % da2geo [ infile ] [-o outfile ] [ -: ] [ -C | -G ] [ -V ]
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

void glatlon(float lon1, float lat1, float delta, float azim,
            float *lon2, float *lat2, int geocen);

int main(int argc, char *argv[])
{
	FILE	*fp,*fpout;
	char	infile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];

	char	line[MAXLINE], data[MAXLINE];

	float	lon1, lat1, lon2, lat2;
	float	x1, y1, x2, y2;
	float	delta, azim;

	int	i;
	int	n_cols;

	int	lin=FALSE;
	int	lout=FALSE;
	int	xy_toggle=FALSE;
	int	geocentric=TRUE;
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
				case 'G': /* use geographic (spherical Earth) coordinates */
					geocentric=FALSE;
					break;
				case 'C': /* use geocentric (elliptical Earth) coordinates */
					geocentric=TRUE;
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
		n_cols = sscanf(line,"%f %f %f %f%[^\n]\n", &x1, &y1, &delta, &azim, data);
		if (n_cols < 4) {
			fprintf(stderr, "ERROR reading line %d (ignored):\n%s\n",i+1,line);
		} else {
			if (xy_toggle) {
				lon1=y1;
				lat1=x1;
			} else {
				lon1=x1;
				lat1=y1;
			}
		}
		glatlon(lon1, lat1, delta, azim, &lon2, &lat2, geocentric);
		if (lon2 <= -180.) lon2 += 360.;
		if (n_cols == 4) fprintf(fpout, "%9.4f%9.4f\n", lon2, lat2);
		else fprintf(fpout, "%9.4f%9.4f%s\n", lon2, lat2, data);
		i++;
	}

	if (verbose) fprintf(stderr,"Number of lines read in: %d\n",i);

	if (lin) fclose(fp);
	if (lout) fclose(fpout);

	return 0;
}
