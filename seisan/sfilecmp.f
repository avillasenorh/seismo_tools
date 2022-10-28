      program sfilecmp1

c-----------------------------------------------------------------------
c Program: sfilecmp1.f
c-----------------------------------------------------------------------
c Purpose: adds weights and component information to SEISAN S-files
c          from the IGN catalog
c
c          - For BH and HH stations:
c              - P picks ->  HZ or BZ; weight code = 2
c              - S picks ->  component closest to transverse;
c                            weight code = 3
c              - Lg picks -> component closest to transverse;
c                            weigth code = 4
c-----------------------------------------------------------------------
c Arguments: none; reads from stdin SEISAN S-file and station file
c            with component information
c-----------------------------------------------------------------------
c Author:   Antonio Villasenor, ICTJA
c Date:     10-JUL-2009
c-----------------------------------------------------------------------

      parameter (nmax=1000)

      character*256 sfile
      character*256 chnfile
      character*80 line

      character*5 ksta(nmax),kdum1
      character*2 channel(nmax),kdum2

c     real*4 slat(nmax),slon(nmax),elev(nmax)

      character*5 sta
      character*4 phase
      character*1 ic

      logical lex

c - get input Nordic S-file and station file

c     write(*,1001) 'Nordic S-file name: '
      read(*,1000) sfile
c     write(*,1001) 'Channel file name: '
      read(*,1000) chnfile

 1000 format(a)
 1001 format(a,$)
 2000 format(2a)

c - check if input files exist

      inquire(file=sfile,exist=lex)
      if (.not.lex) stop 'S-file does not exist'
      inquire(file=chnfile,exist=lex)
      if (.not.lex) stop 'Channel file does not exist'


c - read station file

      open (unit=14,file=chnfile,status='old',
     &      form='formatted',access='sequential')
      i=1
 10   read (14,1002,end=20,err=8001,iostat=ios) kdum1,
     & kdum2
c    & kdum2,rlat,rlon,relev,bdate,edate

      ksta(i)=kdum1
      channel(i)=kdum2
c     slat(i)=rlat
c     slon(i)=rlon
c     elev(i)=relev
      i=i+1
      go to 10

 20   nsta=i-1
      close (14)
c     write(0,1003) 'Number of stations (lines) in channel file:',nsta

 1003 format(a,i8)
 1002 format(a5,2x,a2)
c1002 format(a5,2x,a2,2x,f10.6,2x,f11.6,2x,f7.1,2(2x,a21))

c - read S-file
      i=0
      ieq=0
      ista=0
   30 open (unit=13,file=sfile,status='old',
     &      form='formatted',access='sequential')
      read (13,1000,end=100,err=8000,iostat=ios) line
      i=i+1
      if (i.eq.1 .or. line(80:80).eq.'1') then
         read(line,2200) elat,elon 
         ieq=1
      elseif (line(10:15).eq.'IPHASW') then
         ista=1
      else
         if (ieq.gt.0 .and. ista.gt.0) then
            if (line(2:6).ne.'     ') then
               sta=line(2:6)
               phase=line(11:14)
c              look for station in station list
               ifound=0
               do j=1,nsta
                  if (sta.eq.ksta(j)) then
                     ifound=j
                     go to 40
                  endif
               enddo
   40          if (ifound.gt.0) then
                  ic=channel(ifound)(1:1)
                  line(7:7)=ic
                  if (phase(1:1).eq.'P') then
                     line(8:8)='Z'
                     if (ic.eq.'H' .or. ic.eq.'B') then
                        line(15:15)='2'
                     elseif (ic.eq.'S') then
                        line(15:15)='3'
                     else
c                       line(15:15)='4'
                        line(15:15)='2'
                     endif
                  endif
                  if (phase(1:1).eq.'S' .or. phase(1:2).eq.'Lg') then
c                 call distaz(elat,elon,slat(ifound),slon(ifound),1,
c    &            dist,az,baz,xdeg,nerr)
c                    if (baz.le.45 .or. baz.gt.315 .or.
c    &                  (baz.gt.135 .and. baz.le.225)) then
c                       line(8:8)='E'
c                    else
c                       line(8:8)='N'
c                    endif
c                    if (ic.eq.'H' .or. ic.eq.'B') then
c                       line(15:15)='3'
c                    elseif (ic.eq.'S') then
c                       line(8:8)='Z'
c                       line(15:15)='4'
c                    else
c                       line(15:15)='4'
c                    endif
                     line(8:8)='N'
                     if (ic.eq.'H' .or. ic.eq.'B') then
                        line(15:15)='3'
                     else
c                       line(15:15)='4'
                        line(15:15)='3'
                     endif
                     if (phase(1:2).eq.'Lg') line(15:15)='4'
                  endif
               else
c                 write(0,2000) 'WARNING: station not found: ',sta
c                 line(15:15)='4'
                  if (phase(1:1).eq.'P') then
                     line(15:15)='2'
                  elseif (phase(1:1).eq.'S') then
                     line(15:15)='3'
                  else
                     line(15:15)='4'
                  endif
               endif
            else
               continue
            endif
         endif
      endif
      write(*,1000) line
      go to 30

  100 continue
      close (13)
 2200 format(23x,f7.3,f8.3)

      go to 8888

 8000 print *,'Error reading S-file.  Iostat=',ios
      go to 8888
 8001 print *,'Error reading station file.  Iostat=',ios
      go to 8888

 8888 stop
      end
