      subroutine line1(card,year,month,day,hour,minute,second,
     & model,dtype,etype,lat,lon,depth,deptype,locind,agency,nsta,
     & rms,mag1,magt1,magag1,mag2,magt2,magag2,mag3,magt3,magag3)

      character*80 card
      integer*2 year,month,day,hour,minute
      real*4 second,lat,lon,depth,rms,mag1,mag2,mag3
      character*1 model,dtype,etype,deptype,locind,magt1,magt2,magt3
      character*3 agency,magag1,magag2,magag3
      character*8 mag_str2, mag_str3

      if (deptype.eq.'f') deptype=' '
      mag_str2='        '
      mag_str3='        '

      if (magt2.ne.' ') then
         write(mag_str2,1001) mag2,magt2,magag2
      endif

      if (magt3.ne.' ') then
         write(mag_str3,1001) mag3,magt3,magag3
         print *,mag_str3
      endif

      write(card,1000) year,month,day,hour,minute,second,
     & model,dtype,etype,lat,lon,depth,deptype,locind,agency,nsta,
     & rms,mag1,magt1,magag1,mag_str2,mag_str3

 1000 format(1x,i4,1x,2i2,1x,2i2,1x,f4.1,3a1,f7.3,f8.3,f5.1,2a1,a3,
     & i3,f4.2,f4.1,a1,a3,a8,a8,'1')
 1001 format(f4.1,a1,a3)

      return
      end
