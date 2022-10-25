CC =		gcc
CFLAGS =	-g
#CFLAGS =	-O
FC =		gfortran
FFLAGS =	-g
#FFLAGS =	-O

BINDIR =	${HOME}/bin

BINARIES =	azi \
		azi_c \
		geo2da \
		da2geo \
		getdist \
		midpoint

all:		$(BINARIES)

azi:	azi.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/azi azi.o distaz.o

azi_c:	azi_c.o gdelaz.o
	$(CC) $(FFLAGS) -o $(BINDIR)/azi_c azi_c.o gdelaz.o -lm

geo2da:	geo2da.o gdelaz.o
	$(CC) $(FFLAGS) -o $(BINDIR)/geo2da geo2da.o gdelaz.o -lm

da2geo:	da2geo.o glatlon.o
	$(CC) $(FFLAGS) -o $(BINDIR)/da2geo da2geo.o glatlon.o -lm

getdist:	getdist.o distaz.o
	$(FC) $(FFLAGS) -o $(BINDIR)/getdist getdist.o distaz.o

midpoint:	midpoint.o gdelaz.o glatlon.o
	$(CC) $(FFLAGS) -o $(BINDIR)/midpoint midpoint.o gdelaz.o glatlon.o -lm

distaz.o:	distaz.f mach

clean:
	/bin/rm -f $(BINARIES) *.o

#-------------------------------------------------------------------------------
#       program dependencies
#------------------------------------------------------------------------------- 
.c.o:
		$(CC) -c $(CFLAGS) $<

.f.o:
		$(FC) -c $(FFLAGS) $<

