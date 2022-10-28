      program stadist

c-----------------------------------------------------------------------
c Program: request.f
c-----------------------------------------------------------------------
c Purpose: template to create data request files using different time
c          windows, for example: 
c          - n seconds before first-arriving P wave and including PP
c          - n seconds before origin time, and a group velocity for
c            end of window
c-----------------------------------------------------------------------
c Arguments:
c
c None, because the taup software also reads the command line arguments.
c 1 = event location file name
c 2 = station file name
c-----------------------------------------------------------------------
c Author:   Antonio Villasenor, ICTJA
c Date:     31-AUG-2009
c-----------------------------------------------------------------------

      parameter (nmax=1000)

      character*256 locfile
      character*256 stafile
      character*256 listfile

      character*80 line

      character*21 bdate,edate
      character*6  kdum1,stnm
      character*2  kdum2
      character*12 kaux1,kaux2

      integer*4 yy,mm,dd,hh,min
      real*4  sec,sec1
      real*4  lat,lon,depth,mb,ms
      integer locfmt
      character*6 ksta(nmax)
      character*2 knetwk(nmax)
      character*3 channel(nmax)
      real*4  slat(nmax),slon(nmax),elev(nmax)
      real*4  dist(nmax),az(nmax),baz(nmax),xdeg(nmax)

      logical lex
      logical lfound

c     write(*,1001) 'Location file name: '
      read(*,1000) locfile
c     write(*,1001) 'Station file name: '
      read(*,1000) stafile
c     write(*,1001) 'List file name: '
      read(*,1000) listfile

 1000 format(a)
 1001 format(a,$)
 2000 format(2a)

c - check if input files exist

      inquire(file=locfile,exist=lex)
      if (.not.lex) stop 'Location file does not exist'
      inquire(file=stafile,exist=lex)
      if (.not.lex) stop 'Station file does not exist'

c - read event file

      open (unit=13,file=locfile,status='old',
     &      form='formatted',access='sequential')
         read (13,1000,err=8000,iostat=ios) line

      close (13)

      ifmt=locfmt(line)
      if (ifmt.lt.0) stop 1

      call readloc(line,ifmt,yy,mm,dd,hh,min,sec,lat,lon,depth,ierr)
      if (ierr.ne.0) stop 1

c     print *,yy,mm,dd,hh,min,sec,lat,lon,depth

c - read station file

      open (unit=14,file=stafile,status='old',
     &      form='formatted',access='sequential')
      i=1
 10   read (14,1002,end=20,err=8001,iostat=ios) kdum1,
     & kdum2,rlat,rlon,relev,bdate,edate

      ksta(i)=kdum1
      knetwk(i)=kdum2
      slat(i)=rlat
      slon(i)=rlon
      elev(i)=relev
      i=i+1
      go to 10

 20   nsta=i-1
      close (14)

 1002 format(a6,1x,a2,2x,f10.6,2x,f11.6,2x,f7.1,2(2x,a21))

c     write(*,1003) 'Number of stations (lines) in station file:',nsta
 1003 format(a,i8)

      if (nsta.lt.1) then
         write(0,1000) 'ERROR: no stations read in'
         stop 1
      endif

      if (lat.eq.0. .and. lon.eq.0.) then
         do i=1,nsta
            dist(i)=100.
         enddo
      else
      call distaz(lat,lon,slat,slon,nsta,dist,az,baz,xdeg,nerr)
      endif

c - read list file 

      open (unit=15,file=listfile,status='old',
     &      form='formatted',access='sequential')
      j=1
 30   read (15,1000,end=40,err=8002,iostat=ios) line

      stnm=line(1:index(line,'.')-1)
      lfound=.false.
      do k=1,nsta
         if (stnm.eq.ksta(k)) then
            lfound=.true.
            write(*,'(a20,f10.4)') line(1:20),dist(k)
            go to 31
         endif
      enddo

 31   if (.not.lfound) then
         write(0,2000) 'ERROR: no coordinates found for: ',stnm
      endif

      go to 30

 40   continue
      close(15)


      go to 8888

 8000 print *,'Error reading location file.  Iostat=',ios
      go to 8888
 8001 print *,'Error reading station file.  Iostat=',ios
      go to 8888
 8002 print *,'Error reading list file.  Iostat=',ios
      go to 8888

 8888 stop
      end
