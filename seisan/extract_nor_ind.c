#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/***********************************************************************
* extract_nor_ind.c
* Reads two nordic files and an index file results of associating
*  the events in the second file to the events in the first file
* For the common events, the program adds the picks of the second
*  nordic file to the first one and writes the output to stdout
* 
* Options:
*
* -I index_file  : maximum origin time difference (seconds)
* -o outfile     : writes output to outfile instead of stdout
* -V             : verbose (default runs silently)
*
*
* Usage: % extract_nor_ind nordic_file -I indfile  [-o outfile ] [ -V ]
***********************************************************************/

#ifndef FILENAME_MAX
#define FILENAME_MAX 1024	/* maximum length of a filename */
#endif

#define CARD_SIZE 82
#define MAX_CARDS 5000

#define MAXLINE	512
#define TRUE	1
#define FALSE	0

#define MAXEQ	1000000   /* maximun number of events in the index file */

int get_event_cards(FILE *fp, char **cards, int offset);
int is_blank(const char *s);

int main(int argc, char *argv[])
{
	FILE	*fp, *fpind, *fpout;
	char	file[FILENAME_MAX], indfile[FILENAME_MAX];
	char	outfile[FILENAME_MAX];

	char	line[MAXLINE];
    char    **cards;
    char    eqline[CARD_SIZE];

	int	*evno;

	int	i, j, k;
    int n;
	int	nevents;
    int offset;

    int counter;
    int previous;

	int	lfile=FALSE;
	int	lout=FALSE;
	int	lind=FALSE;
	int	verbose=FALSE;

	/* usage */

	if (argc <= 3) {
		fprintf(stderr,
		"usage: extract_nor_ind file -I indfile [-o outfile ] [ -V ]\n");
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
            if (! lfile) {
		    	if ((fp=fopen(argv[i],"r")) == NULL) {
		    		fprintf(stderr,"ERROR: cannot open input file: %s\n",argv[i]);
		    		exit(1);
                }
                lfile = TRUE;
                strcpy(file, argv[i]);
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
        fprintf(stderr, "Nordic file : %s\n", file);
        fprintf(stderr, "Index file  : %s\n", indfile);
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

    evno = (int *)malloc(MAXEQ*sizeof(int));

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
		sscanf(line,"%d", &evno[i]);
		i++;
	}
	nevents = i;
	if (verbose) fprintf(stderr,"Number of events in index file: %d\n", nevents);
	fclose(fpind);



	// loop through index file

    counter = 0; // event counter for file1
    previous = 0; // previous event number


    i = 0;
    while (i < nevents) {

         fprintf(stderr, "Event number: %d   Nordic file index: %d\n", i, evno[i]);
        // read event from first file
        offset = evno[i] - previous - 1;
        if (offset < 0) {
            fprintf(stderr, "ERROR: unsorted event numbers in column 1 in line %d: %d\n", i, evno[i]);
            exit(1);
        }
        fprintf(stderr, "Nordic file offset: %d\n", offset);
        n = get_event_cards(fp, cards, offset);
        fprintf(stderr, "Event in Nordic file: %d, cards=%d\n", evno[i], n);
        for (k = 0; k < n ; k++) fprintf(stdout, "%s", cards[k]);
        strcpy(eqline, cards[0]);
        previous = evno[i];

        fprintf(stderr, "%s", eqline);

        i++;
    }

	fclose(fp);
	if (lout) fclose(fpout);

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
