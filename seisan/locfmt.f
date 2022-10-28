      integer function locfmt(line)
      character*(*) line

c 1. "loc" file (output ASCII file from CSS3.0)
c 2. HDF file (output from EHB relocation code)
c 3. NEIC Earthquake Search compressed file format
c 4. EHDF format
c 5. Machine-readable earthquake data report format (mchedr)
c 6. ANSS email alert
c 7. simple ".cat" format

      locfmt=-1

c     0 = "asc" format (plain text output from CSS3.0)
      if (line(33:33).eq.'/' .and. line(36:36).eq.'/' .and.
     &    line(46:46).eq.')') then
         locfmt=0
c     1 = "loc" format (modified plain text output from CSS3.0)
      elseif (line(31:31).eq.'/' .and. line(34:34).eq.'/' .and.
     &         line(48:48).eq.':') then
         locfmt=1
c     2 = HDF format (output from EHB relocation code)
      elseif (line(3:4).eq.'EQ' .and. line(33:33).eq.'.' .and.
     &         line(41:41).eq.'.') then
         locfmt=2
c     3 = QED format (compressed screen output from NEIC 
c         Earthquake Search page)
      elseif (line(2:4).eq.'PDE' .and. line(31:31).eq.'.' .and.
     &         line(39:39).eq.'.') then
         locfmt=3
c     4 = NEIC EHDF file (location only)
      elseif (line(1:2).eq.'GS' .and. (line(26:26).eq.'N' .or.
     &         line(26:26).eq.'S')) then
         locfmt=4
c     5 = MCHEDR format (MACHINE-READABLE EARTHQUAKE DATA REPORT)
      elseif (line(1:2).eq.'HY' .and. line(24:24).eq.'.' .and.
     &         line(33:33).eq.'.') then
         locfmt=5
c     6 = Real-time NEIC email format
      elseif (line(1:1).eq.'E' .and. line(11:12).eq.'US') then
         locfmt=6
c     7 = simple ".cat" format
      elseif (line(21:21).eq.'.' .and. line(29:29).eq.'.' .and.
     &        line(38:38).eq.'.') then
         locfmt=7
c     8 = nordic format
c     elseif (line(19:19).eq.'.' .and. line(27:27).eq.'.' .and.
c    &        line(35:35).eq.'.') then
      elseif (line(19:19).eq.'.' .and. line(80:80).eq.'1') then
         locfmt=8
      endif


      if (locfmt.lt.0) then
         write(0,'(a)') 'ERROR: unknown format for location file'
         write(0,'(a)') line(1:lnblnk(line))
      endif 

      return
      end
