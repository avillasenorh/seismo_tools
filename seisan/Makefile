CC =		gcc
CFLAGS =	-g -O

FC =		gfortran
FFLAGS =	-g -O

BINDIR =	${HOME}/bin
#BINDIR =	.

BINARIES =	ign2nor \
			gse2nor \
			iscisf2nor \
			selnor \
			nor2stalist \
			nor_addchn \
            merge_assoc \
			sfilecmp \
			sfileweigth \
			rmevid \
			extractevid \
			stadist \
			sfile_sta_dist \
			sta2dist \
			fixnullchar

all:	$(BINARIES)

ign2nor:	ign2nor.o line1.o line4.o
	$(FC) $(FFLAGS) -o $(BINDIR)/ign2nor ign2nor.o line1.o line4.o

gse2nor:	gse2nor.o line1.o line4.o
	$(FC) $(FFLAGS) -o $(BINDIR)/gse2nor gse2nor.o line1.o line4.o

iscisf2nor:	iscisf2nor.o line1.o line4.o
	$(FC) $(FFLAGS) -o $(BINDIR)/iscisf2nor iscisf2nor.o line1.o line4.o

selnor:	selnor.o substr.o trim.o
	$(CC) $(FFLAGS) -o $(BINDIR)/selnor selnor.o substr.o trim.o

nor2stalist:	nor2stalist.o
	$(FC) $(FFLAGS) -o $(BINDIR)/nor2stalist nor2stalist.o

nor_addchn:	nor_addchn.o
	$(FC) $(FFLAGS) -o $(BINDIR)/nor_addchn nor_addchn.o

merge_assoc:	merge_assoc.o
	$(CC) $(FFLAGS) -o $(BINDIR)/merge_assoc merge_assoc.o

sfilecmp:	sfilecmp.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/sfilecmp sfilecmp.o distaz.o

sfileweigth:	sfileweigth.o
	$(FC) $(FFLAGS) -o $(BINDIR)/sfileweigth sfileweigth.o

rmevid:	rmevid.o substr.o trim.o
	$(CC) $(CFLAGS) -o $(BINDIR)/rmevid rmevid.o substr.o trim.o

extractevid:	extractevid.o substr.o trim.o
	$(CC) $(CFLAGS) -o $(BINDIR)/extractevid extractevid.o substr.o trim.o

stadist:	stadist.o readloc.o locfmt.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/stadist stadist.o readloc.o locfmt.o distaz.o

sfile_sta_dist:	sfile_sta_dist.o readloc.o locfmt.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/sfile_sta_dist sfile_sta_dist.o readloc.o locfmt.o distaz.o

sta2dist:	sta2dist.o readloc.o locfmt.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/sta2dist sta2dist.o readloc.o locfmt.o distaz.o

fixnullchar:	fixnullchar.o
	$(FC) $(FFLAGS) -o $(BINDIR)/fixnullchar fixnullchar.o

clean:
	/bin/rm -f $(BINARIES) *.o

#-------------------------------------------------------------------------------
#       program dependencies
#------------------------------------------------------------------------------- 
.c.o:
		$(CC) -c $(CFLAGS) $<

.f.o:
		$(FC) -c $(FFLAGS) $<
