      program gse2nor

      parameter (maxcards=1000)

      character*132 line
      character*80 card1,cardi,wcard
      character*80 card(maxcards)

      integer*2 year,month,day,hour,minute
      real*4 second,lat,lon,depth,rms,mag1,mag2,mag3
      character*1 model,dtype,etype,deptype,locind,magt1,magt2,magt3
      character*3 agency,magag1,magag2,magag3
      integer*2 az,gap

      character*14 evid

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
      deptype=' '
      locind=' '

      inst=' '
      cmp=' '
      auto=' '
      onset=' '
      pol=' '

      do i=1,80
         wcard(i:i)=' '
      enddo

      open(1,file='test.gse',status='old',form='formatted',err=8888)

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

         write(*,1000) card1
         write(*,'(3a)') 
     &   ' ACTION:                   OP:     STATUS:               ID:',
     &   evid,' L   I'
         write(*,'(2a)') ' STAT SP IPHASW D HRMM SECON CODA AMPLIT ',
     &   'PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
         evid='              '
      elseif (line(5:5).eq.'/') then
         if (levent) then
c           read(line,1001,err=8000,iostat=ios) year,month,day,
c    &      hour,minute,second,err,rms,lat,lon,smaj,smin,az,depth,
c    &      deptype,ndef,nsta,gap,agency

            read(line,1001,err=8000,iostat=ios) year,month,day,
     &      hour,minute,second,lat,lon,depth,ndef,nsta,gap,
     &      magt1,mag1,agency
 1001 format(i4,4(1x,i2),1x,f4.1,4x,f8.4,1x,f9.4,
     & 4x,f5.1,4x,i4,1x,i4,1x,i3,3x,a1,1x,f3.1,27x,a3)

            magag1=agency

            write(evid,'(i4,5i2)') year,month,day,hour,minute,
     &      int(second)
            do i=1,14
               if (evid(i:i).eq.' ') evid(i:i)='0'
            enddo
            nmag=0
            mag2=0.
            mag3=0.
            magt2=' '
            magt3=' '
         else
            write(0,*) 'ERROR: hypocenter line without header:',line
         endif
         levent=.FALSE.
      else
         if (lmag) then
            if (line(1:1).eq.'m' .or. line(1:1).eq.'m') then
               nmag=nmag+1
               read(line,'(a6,2f4.1,1x,i4,1x,a3)')
     &         magname,mag,magerr,nstamag,magsrc
               if (magname(1:4).eq.'mb  ') then
                  km='b'
               elseif (magname(1:4).eq.'mbLg') then
                  km='G'
               elseif (magname(1:4).eq.'Mc  ') then
                  km='C'
               else
                  write(*,*) 'Unknown magnitude',magname
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
                  write(*,*) 'Too many magnitudes',line
               endif
            else
               lmag=.FALSE.
            endif
         elseif (lsta) then
            if (lnblnk(line).gt.0) then
             if (line(10:10).eq.'.') then
               ncard=ncard+1
               read(line,1002) sta,dist,evaz,onset,phase,hh,mm,ss,tres

 1002 format(a5,1x,f6.0,1x,f5.0,3x,a1,1x,a6,12x,2(1x,i2),1x,f4.0,
     & 1x,f5.0)

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

c              if (phase(1:1).eq.'P') cmp='Z'
c              if (phase(1:1).eq.'S') cmp='N'
c              if (phase(1:1).eq.'L') cmp='E'

               call line4(card(ncard),sta,inst,cmp,onset,phase,weight,
     &         auto,pol,hh,mm,ss,amp,per,tres,dist,evaz)
               write(*,1000) card(ncard)
             endif
            else
               write(*,1000) wcard
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
