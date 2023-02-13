#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************
* merge_assoc.c
* Reads input earthquake file from stdin and writes lines in index file
* Optionally, if an argument (not starting with the "-" character)
*  is passed, it is assumed to be the input earthquake file.
* Optionally, if the -o option is used, the next argument is
*  used as the output "extracted" file
*
* Options:
*
* -I index_file  : maximum origin time difference (seconds)
* -o outfile     : writes output to outfile instead of stdout
* -V             : verbose (default runs silently)
*
*
* Usage: % extrind [ infile ] [-o outfile ] [ -I indfile ] [ -V ]
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define CARD_SIZE 82
#define MAX_CARDS 5000

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

#define MAXEQ	25000

int get_event_cards(FILE *fp, char **cards, int offset);
int is_blank(const char *s);

int main(int argc, char *argv[])
{
	FILE	*fp1, *fp2, *fpind, *fpout;
	char	file1[FILENAME_MAX], file2[FILENAME_MAX], indfile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];

	char	line[MAXLINE];
    char    **cards;
    char    eqline1[CARD_SIZE], eqline2[CARD_SIZE];

	int	*evno1, *evno2;

	int	i, j, k;
    int n;
	int	nevents;
    int offset;

    int counter1, counter2;
    int previous1, previous2;

	int	lfile1=FALSE;
	int	lfile2=FALSE;
	int	lout=FALSE;
	int	lind=FALSE;
	int	verbose=FALSE;

	/* usage */

	if (argc <= 4) {
		fprintf(stderr,
		"usage: merge_assoc file1 file2 -I indfile [-o outfile ] [ -V ]\n");
		exit(1);
	}

	/* decode flag options */

	for (i=1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
				case 'I': /* reference file */
					if (argv[i][2] != '\0') strcpy(indfile,&argv[i][2]);
					else strcpy(indfile,argv[++i]);
					lind=TRUE;
					break;
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
            if (! lfile1) {
		    	if ((fp1=fopen(argv[i],"r")) == NULL) {
		    		fprintf(stderr,"ERROR: cannot open input file: %s\n",argv[i]);
		    		exit(1);
                }
                lfile1 = TRUE;
                strcpy(file1, argv[i]);
			} else if (! lfile2) {
		    	if ((fp2=fopen(argv[i],"r")) == NULL) {
		    		fprintf(stderr,"ERROR: cannot open input file: %s\n",argv[i]);
		    		exit(1);
                }
                lfile2 = TRUE;
                strcpy(file2, argv[i]);
            } else {
                fprintf(stderr, "ERROR: too many input files: %s\n",argv[i]);
                exit(1);
            }
		}
	}

	/* if no reference file passed, exit */

	if (! lind) {
		fprintf(stderr,"ERROR: no index file passed\n");
		exit(1);
	} else  {
		if ((fpind=fopen(indfile, "r")) == NULL) {
			fprintf(stderr,"ERROR: cannot open index file: %s\n", indfile);
			exit(1);
		}
	}
    if (verbose) {
        fprintf(stderr, "First Nordic file : %s\n", file1);
        fprintf(stderr, "Second Nordic file: %s\n", file2);
        fprintf(stderr, "Index file        : %s\n", indfile);
    }

	/* if no output file passed, write to stdout */

	if (! lout) fpout=stdout;
	else {
		if ((fpout=fopen(outfile, "w")) == NULL) {
			fprintf(stderr,"ERROR: cannot open output file: %s\n", outfile);
			exit(1);
		}
	}

	// allocate arrays for input file

    evno1 = (int *)malloc(MAXEQ*sizeof(int));
    evno2 = (int *)malloc(MAXEQ*sizeof(int));

    cards = (char **)malloc(sizeof(char *) * MAX_CARDS);
    cards[0] = (char *)malloc(sizeof(char) * MAX_CARDS * CARD_SIZE);
    for (i = 1; i < MAX_CARDS; i++) cards[i] = cards[i-1] + CARD_SIZE;

	// read indices file
	i=0;
	while (fgets(line,MAXLINE,fpind) != NULL) {
		if (i >= MAXEQ) {
			fprintf(stderr,"ERROR: number of indices greater than MAXEQ: %d\n", i);
			exit(1);
		}
		sscanf(line,"%d %d", &evno1[i], &evno2[i]);
		i++;
	}
	nevents = i;
	if (verbose) fprintf(stderr,"Number of events in index file: %d\n", nevents);
	fclose(fpind);



	// loop through index file

    counter1 = 0; // event counter for file1
    counter2 = 0; // event counter for file2

    previous1 = 0; // previous non-zero event in column 1
    previous2 = 0; // previous non-zero event in column 2

    i = 0;
    j = 0;
    while (i < nevents) {

         fprintf(stderr, "Event number: %d   File 1 index: %d   File 2 index: %d\n", i, evno1[i], evno2[i]);
        // read event from first file
        offset = evno1[i] - previous1 - 1;
        if (offset < 0) {
            fprintf(stderr, "ERROR: unsorted event numbers in column 1 in line %d: %d\n", i, evno1[i]);
            exit(1);
        }
        fprintf(stderr, "File 1 offset: %d\n", offset);
        n = get_event_cards(fp1, cards, offset);
        fprintf(stderr, "Event in file 1: %d, cards=%d\n", evno1[i], n);
        for (k = 0; k < n - 1 ; k++) fprintf(stdout, "%s", cards[k]);
        strcpy(eqline1, cards[0]);
        previous1 = evno1[i];

        // if no event in second file skip
        if (evno2[i] == 0) {
            fprintf(stdout, "%s", cards[n-1]);
            fprintf(stderr, "No event in file 2\n");
            i++;
            continue;
        }

        // read event in second file
        offset = evno2[i] - previous2 - 1;
        if (offset < 0) {
            fprintf(stderr, "ERROR: unsorted event numbers in column 2 in line %d: %d\n", i, evno2[i]);
            exit(1);
        }
        fprintf(stderr, "File 2 offset: %d\n", offset);
        n = get_event_cards(fp2, cards, offset);
        fprintf(stderr, "Event in file 2 %d, cards=%d\n", evno2[i], n);
        for (k = 0; k < n ; k++) {
            if (cards[k][79] == ' ' || is_blank(cards[k])) {
                fprintf(stdout, "%s", cards[k]);
            }
        }
        strcpy(eqline2, cards[0]);
        previous2 = evno2[i];

        fprintf(stderr, "%s%s", eqline1, eqline2);

        i++;
    }

	fclose(fp1);
	fclose(fp2);
	if (lout) fclose(fpout);

    if (verbose) fprintf(stderr, "Events to merge: %d\n", j);
//	if (j != nindd) fprintf(stderr, "WARNING: discrepancy between index file and lines written\n");

	return 0;
}


int get_event_cards(FILE *fp, char **cards, int offset) {

    char line[CARD_SIZE];
    int ncards = 0;
    int event = 0;

    int is_blank(const char *s);

    while (fgets(line, CARD_SIZE, fp) != NULL) {
        if (ncards == 0 && line[79] != '1') {
            fprintf(stderr, "ERROR: expected hypocenter line:\n%s", line);
            exit(1);
        }
        strcpy(cards[ncards], line);
        ncards++;
        if (is_blank(line)) {
//          fprintf(stderr, "event=%d, offset=%d, ncards=%d\n", event, offset, ncards);
            if (event == offset) {
                return ncards;
            } else {
                ncards = 0;
                event++;
            }
        }
    }
    
}

int is_blank(const char *s) {
    while(*s) {
        if (! isspace(*s)) return 0;
        s++;
    }
    return 1;
}
