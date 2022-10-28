      program nor2stalist

      parameter (maxsta=1000)

      character*132 infile

      character*80 line
      character*5 sta(maxsta)
      character*5 ksta
      character*2 phase

      integer*4 np(maxsta),ns(maxsta),nl(maxsta)
      integer*4 date1(maxsta),date2(maxsta)
      integer*4 idate

      integer*2 year,month,day,hour,minute
      real*4 second,lat,lon,depth

      integer*2 weight,hh,mm

      logical levent,lsta

c     initialize

      do i=1, maxsta
         sta(i)='     '
         np(i)=0
         ns(i)=0
         nl(i)=0
         date1(i)=0
         date2(i)=0
      enddo

      write (*,*) 'Input NORDIC file:'
      read (*,1000) infile
      open(1,file=infile,status='old',form='formatted',err=8888)

      levent=.FALSE.
      lsta=.FALSE.
      nsta=0
   10 read(1,1000,end=100,iostat=ios) line

      if (line(80:80).eq.'1') then
         read(line,1100,iostat=ios) year,month,day,hour,minute,second,
     &   lat,lon,depth
         idate=10000*year + 100*month + day
c        idate=100*year + month
 1100    format(1x,i4,1x,2i2,1x,2i2,f5.0,3x,f7.3,f8.3,f5.1)
         levent=.TRUE.
      else
         if (line(26:26).eq.'.' .and. line(80:80).eq.' ') then
           read(line,'(1x,a5,4x,a2)') ksta,phase

           ii=-1
           do j=1,nsta
              if (ksta.eq.sta(j)) ii=j
           enddo
           if (ii.lt.0) then
              nsta=nsta+1
              sta(nsta)=ksta
              date1(nsta)=idate
              date2(nsta)=idate
              ii=nsta
           else
              if (idate.lt.date1(ii)) date1(ii)=idate
              if (idate.gt.date2(ii)) date2(ii)=idate
           endif

           if (phase(1:1).eq.'P') np(ii)=np(ii)+1
           if (phase(1:1).eq.'S') ns(ii)=ns(ii)+1
           if (phase(1:1).eq.'L') nl(ii)=nl(ii)+1
         endif
      endif

      go to 10

  100 continue
      close(1)

      do i=1,nsta
         write(*,'(a5,3(1x,i8),2(2x,i8))') sta(i),np(i),ns(i),nl(i),
     &   date1(i),date2(i)
      enddo

      go to 9999
 1000 format(a)
 2000 format(2a)

c - error messages

 8000 write(0,2000) 'ERROR reading line: ',line
      go to 9999

 8888 write(0,1000) 'ERROR opening input IMS file'
      go to 9999

 9999 stop
      end
