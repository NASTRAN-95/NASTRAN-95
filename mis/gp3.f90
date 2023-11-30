
SUBROUTINE gp3
   IMPLICIT NONE
   INTEGER Buf(50) , Buf1 , Buf2 , Buf3 , Geom3 , Gptt , Idno(30) , Igrav , Ipld3 , Ipload , Load(2) , Mask(60) , Nograv , Noload , &
         & Nopld2 , Notemp , Ntypes , Pload2(2) , Slt , Sperlk , Status(60) , Sysbuf , Two(32)
   REAL Carddt(60) , Cardid(60) , Eqexin , Geom2 , Pload3(2) , Scr1 , Scr2 , Sy(93) , Temp(2) , Tempd(2) , Tempp1(2) , Tempp2(2) ,  &
      & Tempp3(2) , Temprb(2) , Z(1)
   COMMON /blank / Nograv , Noload , Notemp
   COMMON /gp3com/ Geom3 , Eqexin , Geom2 , Slt , Gptt , Scr1 , Scr2 , Buf1 , Buf2 , Buf , Cardid , Idno , Carddt , Mask , Status , &
                 & Ntypes , Ipload , Igrav , Pload2 , Load , Nopld2 , Temp , Tempd , Tempp1 , Tempp2 , Tempp3 , Temprb , Buf3 ,     &
                 & Pload3 , Ipld3
   COMMON /system/ Sysbuf , Sy , Sperlk
   COMMON /two   / Two
   COMMON /zzzzzz/ Z
   INTEGER i
   INTEGER korsz
!
!     GP3 IS THE MAIN CONTROL PROGRAM FOR MODULE GP3.
!     IF PLOAD2 CARDS ARE PRESENT, GP3C IS EXECUTED TO BUILD PLOAD DATA
!     ON SCRATCH FILE 2 (SCR2). GP3A IS EXECUTED TO BUILD THE STATIC
!     LOADS TABLE (SLT). GP3B IS EXECUTED TO BUILD THE GRID POINT
!     TEMPERATURE TABLE (GPTT).
!     GP3D IS EXECUTED TO BUILD THE ELEMENT TEMPERATURE TABLE (ETT) FROM
!     THE GPTT AND ANY TEMPP1,TEMPP2,TEMPP3, AND TEMPRB DATA PRESENT.
!
!
!     TURN PARAMETERS ON. INITIALIZE BUFFER POINTERS.
!     READ TRAILER ON GEOM3. IF PURGED, EXIT.
!
   CALL delset
!
   IF ( Sperlk/=0 ) THEN
      DO i = 1 , 60 , 2
         Status(i) = -1
         Status(i+1) = 0
      ENDDO
   ENDIF
   Noload = -1
   Nograv = -1
   Notemp = -1
   Buf1 = korsz(Z) - Sysbuf - 2
   Buf2 = Buf1 - Sysbuf
   Buf3 = Buf2 - Sysbuf - 2
   Buf(1) = Geom3
   CALL rdtrl(Buf)
   IF ( Buf(1)/=Geom3 ) RETURN
!
!     IF THE SLT IS PURGED, BYPASS THE SLT PHASE OF GP3.
!     OTHERWISE, IF PLOAD2 CARDS PRESENT, EXECUTE GP3C.
!     EXECUTE GP3A TO COMPLETE SLT PHASE.
!
   Buf(7) = Slt
   CALL rdtrl(Buf(7))
   IF ( Buf(7)==Slt ) THEN
      CALL gp3c
      CALL gp3a
   ENDIF
!
!     IF THE GPTT IS NOT PURGED, EXECUTE GP3B TO BUILD IT.
!
   Buf(7) = Gptt
   CALL rdtrl(Buf(7))
   IF ( Buf(7)/=Gptt ) RETURN
!
!     GP3B WILL FORM A GPTT ON SCR1 AND THEN GP3D WILL READ SCR1 AND
!     THE TEMPP1,TEMPP2,TEMPP3, AND TEMPRB DATA FROM GEOM3 TO FORM THE
!     ETT (ELEMENT TEMPERATURE TABLE) ON THE OUTPUT FILE GPTT.
!
   CALL gp3b
   CALL gp3d
END SUBROUTINE gp3
