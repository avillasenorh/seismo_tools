      program azi

      character*256 infile,outfile
      character*256 line

      logical lout,lex,segment

c     subroutine distaz(the,phe,ths,phs,ns,dist,az,baz,xdeg,nerr)
c=====================================================================
c PURPOSE:  To compute the distance and azimuth between locations.
c=====================================================================
c INPUT ARGUMENTS:
c    THE:     Event latitude in decimal degrees, North positive. [r]
c    PHE:     Event longitude, East positive. [r]
c    THS:     Array of station latitudes. [r]
c    PHS:     Array of station longitudes. [r]
c    NS:      Length of THS and PHS. [i]
c=====================================================================
c OUTPUT ARGUMENTS:
c    DIST:    Array of epicentral distances in km. [r]
c    AZ:      Array of azimuths in degrees. [r]
c    BAZ:     Array of back azimuths. [r]
c    XDEG:    Array of great circle arc lengths. [r]
c    NERR:    Error flag:
c             =    0   No error.
c             = 0904   Calculation failed internal consistency checks.
c=====================================================================

      numarg=iargc()

      if (numarg.eq.2) then
         call getarg(1,infile)
         call getarg(2,outfile)
         lout=.true.
      else if (numarg.eq.1) then
         call getarg(1,infile)
         lout=.false.
      else
         write(0,'(a)') 'Usage: azi infile [outfile]'
         stop 1
      endif

c - check if input files exist

      inquire(file=infile,exist=lex)
      if (.not.lex) stop 'input file does not exist'

      open (unit=1,file=infile,status='old',
     &      form='formatted',access='sequential')

      if (lout) then
         lu=2
         open (unit=lu,file=outfile,status='unknown',
     &      form='formatted',access='sequential')
      else
         lu=6
      endif

c - read input file

      segment=.false.
      npt=1

   10 read(1,1000,end=100) line

      if (line(1:1).eq.'>') then
         segment=.false.
      else
         read(line,*) rlon,rlat
         if (.not.segment) then
            segment=.true.
         else
             call distaz(rlat1,rlon1,rlat,rlon,
     &       npt,dist,az,baz,xdeg,nerr)
             if (nerr.eq.0) then
c               write(lu,1001) rlon1,rlat1,rlon,rlat,az,dist,xdeg
                write(lu,1002) az,dist,xdeg,rlon1,rlat1,rlon,rlat
             endif
         endif

         rlon1=rlon
         rlat1=rlat

      endif


      go to 10

  100 continue

      close(1)
      if (lout) close(lu)

 1000 format(a)
 1001 format(4f10.4,1x,f6.2,1x,f8.3,1x,f6.2)
 1002 format(f6.2,1x,f8.3,1x,f8.4,1x,4f10.4)

      stop
      end
