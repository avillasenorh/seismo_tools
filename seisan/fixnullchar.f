      program fixnullchar

c ----------------------------------------------------------------------
c ----------------------------------------------------------------------
c ----------------------------------------------------------------------

      character*256 norfile
      character*256 line

      logical lex

      numarg=iargc()

      if (numarg.ge.1) then
         call getarg(1,norfile)
      else
         write(0,'(a)') 'Usage: fixnullchar norfile'
         stop 1
      endif

c - check if input file exists

      inquire(file=norfile,exist=lex)
      if (.not.lex) stop 'input NORDIC file does not exist'

c - read NORDIC file

      open (unit=1,file=norfile,status='old',
     &      form='formatted',access='sequential')

   10  read (1,1000,end=100) line

       do i=1,len(line)
          if (ichar(line(i:i)).eq.0) line(i:i)=' '
       enddo
       write(*,1000) line(1:80)
c      write(0,'(i3)') lnblnk(line)

       go to 10

  100 continue
      close(1)

 1000 format(a)
      stop
      end
