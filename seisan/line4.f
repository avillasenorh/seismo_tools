      subroutine line4(card,sta,inst,cmp,onset,phase,weight,
     &         auto,pol,hh,mm,ss,amp,per,tres,dist,evaz)

      character*80 card
      character*5 sta
      character*8 phase
      character*1 inst,cmp,onset,auto,pol
      integer*2 weight,hh,mm
      real*4 ss,amp,per,tres,dist,evaz

      character*4 ph

      ph=phase(1:4)

      if (tres.gt.99.99) tres=99.99
      if (tres.lt.-9.99) tres=-9.99

      if (amp.gt.99999.9) amp=99999.9
      if (per.gt.99.9) per=99.9

      write(card,1000) sta,inst,cmp,onset,ph,weight,auto,pol,
     & hh,mm,ss,tres,nint(dist),nint(evaz)
 1000 format(1x,a5,2a1,1x,a1,a4,i1,2a1,1x,2i2,f6.2,35x,
     & f5.2,2x,i5,1x,i3)

      if (per.gt.0. .and .per.le.9.99) then
         write(card(34:45),'(f7.1,1x,f4.2)') amp,per
      elseif (per.gt.9.99) then
         write(card(34:45),'(f7.1,1x,f4.1)') amp,per
      endif

c     write(card,1000) sta,inst,cmp,onset,ph,weight,auto,pol,
c    & hh,mm,ss,amp,per,tres,nint(dist),nint(evaz)
c1000 format(1x,a5,2a1,1x,a1,a4,i1,2a1,1x,2i2,f6.2,5x,
c    & f7.1,1x,f4.1,18x,f5.2,2x,i5,1x,i3)

      card(80:80)=' '

      return
      end
