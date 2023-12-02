!*==gp3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gp3
   IMPLICIT NONE
   USE C_BLANK
   USE C_GP3COM
   USE C_SYSTEM
   USE C_TWO
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i
   EXTERNAL delset , gp3a , gp3b , gp3c , gp3d , korsz , rdtrl
!
! End of declarations rewritten by SPAG
!
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
