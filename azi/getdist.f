      program getdist

      character*256 line
      character*256 kchar

      logical lswap

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

      lswap=.false.
      numarg=iargc()
      if (numarg.ge.1) then
         call getarg(1,kchar)
         if (kchar(1:2).eq.'-:') lswap=.true.
      endif

c - read data from stdin

   10 read(5,1000,end=100) line

      if (lswap) then
         read(line,*,iostat=ios) rlat1,rlon1,rlat2,rlon2
      else
         read(line,*,iostat=ios) rlon1,rlat1,rlon2,rlat2
      endif

      if (rlat1.gt.90. .or. rlat1.lt.-90. .or.
     &    rlat2.gt.90. .or. rlat2.lt.-90.) then
         write(0,1010) 'ERROR: invalid latitude: ',line(1:lnblnk(line))
         go to 10
      endif

      if (rlon1.gt.360. .or. rlon1.lt.-180. .or.
     &    rlon2.gt.360. .or. rlon2.lt.-180.) then
         write(0,1010) 'ERROR: invalid longitude: ',line(1:lnblnk(line))
         go to 10
      endif

      if (rlon1.gt.180.) rlon1=rlon1-360.
      if (rlon2.gt.180.) rlon2=rlon2-360.

      npt=1
      call distaz(rlat1,rlon1,rlat2,rlon2,npt,dist,az,baz,xdeg,nerr)

      if (nerr.eq.0) then
c        write(6,1001) rlon1,rlat1,rlon,rlat,az,dist,xdeg
c        write(6,1002) az,dist,xdeg,rlon1,rlat1,rlon2,rlat2
         write(6,1003) az,xdeg,dist,rlon1,rlat1,rlon2,rlat2
      else
         write(0,1010) 'ERROR: ',line(1:lnblnk(line))
      endif

      go to 10

  100 continue


 1000 format(a)
 1001 format(4f10.4,1x,f6.2,1x,f8.3,1x,f6.2)
 1002 format(f6.2,1x,f8.3,1x,f8.4,1x,4f10.4)
 1003 format(f6.2,1x,f8.4,1x,f9.3,1x,4f10.4)
 1010 format(2a)

      stop
      end
