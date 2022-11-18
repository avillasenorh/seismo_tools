      program nor2stalist

      parameter (maxsta=1000)

      character*132 infile
      character*132 chnfile

      character*80 line
      character*5 sta(maxsta)
      character*5 ksta
      character*2 phase

      character*5 code(3*maxsta)
      character*3 chn(3*maxsta)

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
      write (*,*) 'Input channel file:'
      read (*,1000) chnfile
      open(2,file=chnfile,status='old',form='formatted',err=8888)

      i=1
    5 read(2,'(a5,2x,a3)',end=50,iostat=ios) code(i), chn(i)
      i=i+1
      go to 5
   50 nchn=i-1
      close(2)
      print *,'Number of channels in channel file:',nchn

      levent=.FALSE.
      lsta=.FALSE.
      nsta=0
   10 read(1,1000,end=100,iostat=ios) line

      if (line(80:80).eq.'1') then
         read(line,1100,iostat=ios) year
 1100    format(1x,i4)
         levent=.TRUE.
      else
         if (line(26:26).eq.'.' .and. line(80:80).eq.' ') then
           read(line,'(1x,a5,4x,a2)') ksta,phase
           ii=-1
           do j=1,nchn
              if (ksta.eq.code(j)) then
                 ii=j
                 go to 750
              endif
           enddo

  750      if (ii.lt.0) then
               print *,'Channels not found for ',ksta
           else
               if (line(7:7).eq.' ') then
                  line(7:7)=chn(ii)(1:1)
               else
                  if (phase(1:1).eq.'P') then
                     line(8:8)='Z'
                  elseif (phase(1:1).eq.'S') then
                     line(8:8)=chn(ii)(3:3)
                  endif

                endif

           endif
         endif

      endif

      write(*,'(a)') line

      go to 10

  100 continue
      close(1)

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
