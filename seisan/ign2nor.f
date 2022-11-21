      program ign2nor

      parameter (maxcards=1000)

      character*512 imsfile
      character*512 norfile
      character*132 line
      character*80 card1,cardi,wcard
      character*80 card(maxcards)

      integer*2 year,month,day,hour,minute
      real*4 second,lat,lon,depth,rms,mag1,mag2,mag3
      character*1 model,dtype,etype,deptype,locind,magt1,magt2,magt3
      character*3 agency,magag1,magag2,magag3
      integer*2 az,gap

      character*14 evid
      character*8 arrid

      character*6 magname
      real*4 mag,magerr
      integer*2 nstamag
      character*3 magsrc
      character*1 km

      character*5 sta
      character*8 phase
      character*1 inst,cmp,onset,auto,pol
      integer*2 weight,hh,mm
      real*4 ss,amp,per,tres,dist,evaz

      logical levent,lmag,lsta

      model=' '
      dtype='L'
      etype=' '
      locind=' '

      inst=' '
      auto=' '
      onset=' '
      pol=' '

      do i=1,80
         wcard(i:i)=' '
      enddo

      write(*,1100) 'Enter input IMS file: '
      read(*,1000) imsfile
      write(*,1100) 'Enter output nordic file: '
      read(*,1000) norfile

      open(1,file=imsfile,status='old',form='formatted',err=8888)
      open(2,file=norfile,status='unknown',form='formatted')

      levent=.FALSE.
      lmag=.FALSE.
      lsta=.FALSE.
   10 read(1,1000,end=100,iostat=ios) line

      if (line(1:7).eq.'   Date') then
         levent=.TRUE.
      elseif (line(1:9).eq.'Magnitude') then
         lmag=.TRUE.
         levent=.FALSE.
      elseif (line(1:4).eq.'Sta ') then
         ncard=0
         lsta=.TRUE.
         levent=.FALSE.
         lmag=.FALSE.
         call line1(card1,year,month,day,hour,minute,second,
     &   model,dtype,etype,lat,lon,depth,deptype,locind,agency,nsta,
     &   rms,mag1,magt1,magag1,mag2,magt2,magag2,mag3,magt3,magag3)

         write(2,1000) card1
         write(2,'(3a)') 
     &   ' ACTION:NEW                OP:avh  STATUS:               ID:',
     &   evid,' L   I'
         write(2,'(2a)') ' STAT SP IPHASW D HRMM SECON CODA AMPLIT ',
     &   'PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
         evid='              '
      elseif (line(5:5).eq.'/') then
         if (levent) then
            if (line(32:32).ne.'.') then

            read(line,1001,err=8000,iostat=ios) year,month,day,
     &      hour,minute,second,err,rms,lat,lon,smaj,smin,az,depth,
     &      deptype,ndef,nsta,gap,agency

            else

            read(line,1011,err=8000,iostat=ios) year,month,day,
     &      hour,minute,second,err,rms,lat,lon,smaj,smin,az,depth,
     &      deptype,ndef,nsta,gap,agency

            endif

            if (agency.eq.'bul') agency='IGN'

            write(evid,'(i4,5i2)') year,month,day,hour,minute,
     &      int(second)
            do i=1,14
               if (evid(i:i).eq.' ') evid(i:i)='0'
            enddo
            nmag=0
            mag1=0.
            mag2=0.
            mag3=0.
            magt1=' '
            magt2=' '
            magt3=' '
            magag1='   '
            magag2='   '
            magag3='   '
         else
            write(0,*) 'ERROR: hypocenter line without header:',line
         endif
         levent=.FALSE.
      else
         if (lmag) then
            if (line(1:1).eq.'M' .or. line(1:1).eq.'m') then
               nmag=nmag+1
               read(line,'(a6,2f4.1,1x,i4,1x,a3)')
     &         magname,mag,magerr,nstamag,magsrc
               if (magname(1:4).eq.'mb  ') then
                  km='b'
               elseif (magname(1:5).eq.'mb_VC') then
                  km='b'
               elseif (magname(1:5).eq.'mb_Lg') then
                  km='G'
               elseif (magname(1:4).eq.'mbLg') then
                  km='G'
               elseif (magname(1:4).eq.'M_mb') then
                  km='W'
               elseif (magname(1:4).eq.'Mc  ') then
                  km='C'
               elseif (magname(1:4).eq.'Md  ') then
                  km='C'
               elseif (magname(1:4).eq.'Ms  ') then
                  km='S'
               elseif (magname(1:4).eq.'ML  ') then
                  km='L'
               elseif (magname(1:4).eq.'Ml  ') then
                  km='L'
               elseif (magname(1:4).eq.'MLv ') then
                  km='L'
               else
                  write(0,*) 'Unknown magnitude: ',magname
                  write(0,'(a)') line
                  km=' '
               endif
               if (magsrc.eq.'bul') magsrc='IGN'
               if (nmag.eq.1) then
                  mag1=mag
                  magt1=km
                  magag1=magsrc
               elseif (nmag.eq.2) then
                  mag2=mag
                  magt2=km
                  magag2=magsrc
               elseif (nmag.eq.3) then
                  mag3=mag
                  magt3=km
                  magag3=magsrc
               else
                  write(0,*) 'Too many magnitudes',line
               endif
c           else
c              lmag=.FALSE.
            endif
         elseif (lsta) then
            if (lnblnk(line).gt.0) then
             if (line(10:10).eq.'.' .and. line(20:21).ne.'IV') then
               ncard=ncard+1
               read(line,1002) sta,dist,evaz,phase,hh,mm,ss,tres,
     &         amp,per,arrid

c              before 2016-02-18 Sg is reported as Lg

               if (phase(1:4).eq.'Lg  ' .and. year.le.2016) then
                   phase='Sg      '
               endif

               dist=dist*111.11
               if (dist.le.50. .and. phase(1:1).eq.'P') then
                  weight=1
               elseif (dist.gt.50. .and. phase(1:1).eq.'P') then
                  weight=1
               elseif (dist.le.50. .and. phase(1:1).eq.'S') then
                  weight=2
               elseif (dist.gt.50. .and. phase(1:1).eq.'S') then
                  weight=2
               else
                  weight=4
               endif

               if (year.ge.2016 .and. arrid(3:3).eq.'_') then
                   inst=arrid(6:6)
                   cmp=arrid(8:8)
                   if (cmp.eq.'Z' .and. phase(1:1).eq.'S') then
                       cmp='E'
                   endif
               else
                   inst=' '
                   if (phase(1:1).eq.'P') cmp='Z'
                   if (phase(1:1).eq.'S') cmp='E'
                   if (phase(1:1).eq.'L') cmp='E'
               endif

               call line4(card(ncard),sta,inst,cmp,onset,phase,weight,
     &         auto,pol,hh,mm,ss,amp,per,tres,dist,evaz)
               write(2,1000) card(ncard)
             endif
            else
               write(2,1000) wcard
               lsta=.FALSE.
            endif
         else
            if (line(10:10).eq.'.' .and. line(17:17).eq.'.') then
               write(0,*) 'WARNING: phase line in wrong location:',line
            endif
         endif
      endif


      go to 10

  100 continue
      close(1)
      close(2)

      go to 9999
 1000 format(a)
 1100 format(a,$)
 2000 format(2a)
 1001 format(i4,4(1x,i2),1x,f6.3,1x,f5.2,1x,f5.2,1x,f8.4,1x,f9.4,
     & 2(1x,f5.1),1x,i3,1x,f5.1,a1,6x,i4,1x,i4,1x,i3,22x,a3)
 1011 format(i4,4(1x,i2),1x,f5.2,1x,f5.2,1x,f5.2,1x,f8.4,1x,f9.4,
     & 2(1x,f5.1),1x,i3,1x,f5.1,a1,6x,i4,1x,i4,1x,i3,22x,a3)
 1002 format(a5,1x,f6.3,1x,f5.1,1x,a8,2(1x,i2),1x,f6.3,1x,f5.1,37x,
     & f9.1,1x,f5.2,16x,a8)

c - error messages

 8000 write(0,2000) 'ERROR reading line: ',line
      go to 9999

 8888 write(0,1000) 'ERROR opening input IMS file'
      go to 9999


 9999 stop
      end
