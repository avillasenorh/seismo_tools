      program sfileweight

c-----------------------------------------------------------------------
c Program: sfileweight.f
c-----------------------------------------------------------------------
c Purpose: adds weight information to SEISAN S-files from the IGN catalog
c
c              - P picks ->  weight code = 1
c              - S picks ->  weight code = 2
c              - Lg picks -> weigth code = 4
c-----------------------------------------------------------------------
c Arguments: none; reads from stdin SEISAN S-file and station file
c            with component information
c-----------------------------------------------------------------------
c Author:   Antonio Villasenor, ICTJA
c Date:     29-AUG-2012 (modified from sfilecmp.f)
c-----------------------------------------------------------------------

      parameter (nmax=1000)

      character*256 sfile
      character*256 stafile
      character*80 line

      character*5 sta
      character*4 phase
      character*1 ic

      logical lex

c - get input Nordic S-file

c     write(*,1001) 'Nordic S-file name: '
      read(*,1000) sfile

 1000 format(a)
 1001 format(a,$)
 2000 format(2a)

c - check if input files exist

      inquire(file=sfile,exist=lex)
      if (.not.lex) stop 'S-file does not exist'


c - read S-file
      i=0
      ieq=0
      ista=0
   30 open (unit=13,file=sfile,status='old',
     &      form='formatted',access='sequential')
      read (13,1000,end=100,err=8000,iostat=ios) line
      i=i+1
      if (i.eq.1 .or. line(80:80).eq.'1') then
         ieq=1
      elseif (line(10:15).eq.'IPHASW') then
         ista=1
      else
         if (ieq.gt.0 .and. ista.gt.0) then
            if (line(2:6).ne.'     ') then
               sta=line(2:6)
               phase=line(11:14)
               if (phase(1:1).eq.'P') then
                  line(15:15)='1'
               elseif (phase(1:1).eq.'S') then
                  line(15:15)='2'
               elseif (phase(1:2).eq.'Lg') then
                  line(15:15)='4'
               else
                  line(15:15)='4'
               endif
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

 8888 stop
      end
