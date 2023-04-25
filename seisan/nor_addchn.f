      program nor2stalist

      parameter (maxsta=1000)

      character*132 infile
      character*132 outfile
      character*132 chnfile

      character*132 line(1000)
      character*5 sta(maxsta)
      character*5 ksta
      character*2 phase

      character*5 code(3*maxsta)
      character*3 chn(3*maxsta)

      integer*4 i,ii,k
      integer*4 nchn

c     write (*,*) 'Input NORDIC file:'
      read (*,1000) infile
      open(1,file=infile,status='old',form='formatted',err=8888)
c     write (*,*) 'Ouput NORDIC file:'
      read (*,1000) outfile
      open(3,file=outfile,status='unknown',form='formatted',err=8888)
c     write (*,*) 'Input channel file:'
      read (*,1000) chnfile
      open(2,file=chnfile,status='old',form='formatted',err=8888)

      i=1
    5 read(2,'(a5,2x,a3)',end=50,iostat=ios) code(i), chn(i)
      i=i+1
      go to 5
   50 nchn=i-1
      close(2)
c     print *,'Number of channels in channel file:',nchn

      k=1
   10 read(1,1000,end=100,iostat=ios) line(k)

      if (line(k)(26:26).eq.'.' .and. line(k)(80:80).eq.' ') then

           read(line(k),'(1x,a5,4x,a2)') ksta, phase
           ii=-1
           do j=1,nchn
              if (ksta.eq.code(j)) then
                 ii=j
                 go to 750
              endif
           enddo

  750      continue
           if (ii.lt.0) then
               write(0,'(a,1x,a)') 'Channels not found for',ksta
           else
               if (line(k)(7:7).eq.' ') then
                  line(k)(7:7)=chn(ii)(1:1)
               endif
               if (phase(1:1).eq.'P') then
                   line(k)(8:8)='Z'
               elseif (phase(1:1).eq.'S') then
                   line(k)(8:8)=chn(ii)(3:3)
               endif
           endif
      endif

      k = k + 1
      go to 10

  100 continue
      close(1)

c     print *,'Number of lines: ', k-1

      do i=1,k-1
          write(3,'(a)') line(i)(1:80)
      enddo
      close(3)

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
