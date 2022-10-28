      subroutine readloc(line,ifmt,yy,mm,dd,hh,min,sec,
     & lat,lon,depth,ierr)
      character*(*) line
      integer ifmt,yy,mm,dd,hh,min,ierr
      real*4 sec,lat,lon,depth
      character*1 ins,iew

c 1. "loc" file (output ASCII file from CSS3.0)
c 2. HDF file (output from EHB relocation code)
c 3. NEIC Earthquake Search compressed file format
c 4. EHDF format
c 5. Machine-readable earthquake data report format (mchedr)
c 6. ANSS email alert
c 7. simple ".cat" format
c 8. Nordic format

      ierr=0

      if (ifmt.eq.0) then
         read (line,1010,iostat=ios) lat,lon,depth,
     &   mm,dd,yy,jdum,hh,min,sec
      elseif (ifmt.eq.1) then
         read (line,1001,iostat=ios) lat,lon,depth,
     &   mm,dd,yy,jdum,hh,min,sec
      elseif (ifmt.eq.2) then
         read (line,1002,iostat=ios) iyr,mm,dd,hh,min,sec,
     &   lat,lon,depth
         if (iyr.lt.20) then
             yy=iyr+2000
         else
             yy=iyr+1900
         endif
      elseif (ifmt.eq.3) then
         read (line,1003,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,lon,depth
      elseif (ifmt.eq.4) then
         read (line,1004,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,ins,lon,iew,depth
         if (ins.eq.'S') lat=-lat
         if (iew.eq.'W') lon=-lon
      elseif (ifmt.eq.5) then
         read (line,1005,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,ins,lon,iew,depth
         if (ins.eq.'S') lat=-lat
         if (iew.eq.'W') lon=-lon
      elseif (ifmt.eq.6) then
         read (line,1006,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,lon,depth
      elseif (ifmt.eq.7) then
         read (line,1007,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,lon,depth
      elseif (ifmt.eq.8) then
         read (line,1008,iostat=ios) yy,mm,dd,hh,min,sec,
     &   lat,lon,depth
      else
         write(0,'(a)') 'ERROR: unknown format for location file'
         ierr=-1
         go to 8888
      endif

      ierr=ios

 1001 format(f8.4,1x,f9.4,1x,f8.4,1x,2(i2,'/'),i4,3x,i3,1x,
     & 2(i2,':'),f6.3)
 1002 format(5x,3(1x,i2),1x,2(1x,i2),1x,f5.2,2x,f7.3,f8.3,1x,f5.1)
 1003 format(7x,i4,1x,4i2,f5.2,2x,f7.3,f8.3,f3.0)
 1004 format(4x,i4,4i2,f4.2,f5.3,a1,f6.3,a1,f4.1)
 1005 format(2x,i4,2i2,1x,2i2,f5.2,1x,f6.3,a1,1x,f7.3,a1,1x,f5.1)
 1006 format(13x,i4,4i2,f3.1,f7.4,f8.4,f4.1)
 1007 format(i4,1x,i2,1x,i2,2x,i2,1x,i2,1x,f5.2,2x,
     & f7.3,1x,f8.3,1x,f5.1)
 1008 format(1x,i4,2(1x,2i2),1x,f4.1,3x,f7.3,f8.3,f5.1)
 1010 format(f9.4,1x,f9.4,1x,f9.4,1x,2(i2,'/'),i4,1x,'(',i3,')',1x,
     & 2(i2,':'),f6.3)

 8888 return
      end

