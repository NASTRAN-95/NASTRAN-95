!*==cputim.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cputim(Icpusc,Rcpusc,Iflag)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Icpusc
   REAL :: Rcpusc
   INTEGER :: Iflag
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(2) :: array
   REAL :: save , t , time
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS MACHINE DEPENDENT
!
!     THIS ROUTINE OBTAINS THE CURRENT CPU TIME IN SECONDS.
!     IF IFLAG .EQ. 0, CPU TIME IS RETURNED AS AN INTEGER VALUE.
!     IF IFLAG .NE. 0, CPU TIME IS RETURNED AS A REAL VALUE.
!
!     DESIGN REQUIREMENT:
!     RCPUSC MUST OVER THE RANGE OF 10**-3 TO 10**5 CPU SECONDS
!
!     DEC VAX/VMS VERSION
!     ===================
!
!     INCLUDE '($JPIDEF)'
!
!     INTEGER   BUF_ADDR, ZERO, CPU_TIME
!     INTEGER*2 BUF_LNGTH, ITEM_CODE
!
!     COMMON /CPU_LIST/ BUF_LNGTH, ITEM_CODE, BUF_ADDR, LNGTH_ADDR,
!    1                  ZERO
!
!     DATA BUF_LNGTH, LNGTH_ADDR, ZERO /4, 2*0/
!     DATA ITEM_CODE /JPI$_CPUTIM/
!
!     BUF_ADDR = %LOC (CPU_TIME)
!     CALL SYS$GETJPI (,,,BUF_LNGTH,,,)
!     IF (IFLAG.EQ.0) ICPUSC = CPU_TIME/100
!     IF (IFLAG.NE.0) RCPUSC = CPU_TIME/100.0
!     RETURN
!
!
!     SUBROUTINE CPUTIM (ICPUSC,RCPUSC,IFLAG)
!
!     UNIX VERSION
!     ============
!
!     THIS ROUTINE OBTAINS THE CURRENT CPU TIME IN SECONDS
!     IF IFLAG.EQ.0, CPU TIME IS RETURNED AS AN INTEGER VALUE IN ICPUSC
!     IF IFLAG.NE.0, CPU TIME IS RETURNED AS A REAL VALUE IN RCPUSC,
!
!     DESIGN REQUIREMENT -
!     RCPUSC MUST COVER THE RANGE OF 1.0**-3 TO 1.0**+5 CPU SECONDS
!
!     NOTE - THE CURRENT CALL TO CPUTIM MUST GIVE A TIME VALUE BIGGER
!     THAN PREVIOUS CPUTIME CALL. OTHERWISE, CALLING ROUTINE MAY GET
!     INTO TROUBLE, SUCH AS DIVIDED BY ZERO.
!
!     REAL ARRAY(2)
!
!     CALL ETIME (ARRAY)
!     IF (IFLAG .NE. 0) GO TO 10
!     ICPUSC = ARRAY(2) + .49
!     GO TO 20
!  10 SAVE   = RCPUSC
!     RCPUSC = ARRAY(2)
!     IF (RCPUSC .LE. SAVE) RCPUSC = RCPUSC + 0.0001
!  20 RETURN
!
!
!
!     SUBROUTINE CPUTIM (ICPUSC,RCPUSC,IFLAG)
!
!     UNIVERSAL VERSION
!     =================
!
!     THIS ROUTINE OBTAINS THE CURRENT CPU TIME IN SECONDS
!     IF IFLAG.EQ.0, CPU TIME IS RETURNED AS AN INTEGER VALUE IN ICPUSC
!     IF IFLAG.NE.0, CPU TIME IS RETURNED AS A REAL VALUE IN RCPUSC,
!
!     DESIGN REQUIREMENT -
!     RCPUSC MUST COVER THE RANGE OF 1.0**-3 TO 1.0**+5 CPU SECONDS
!     (SECNDS MAY BE ACCURATE ONLY TO 1/60, OR 0.001 SECOND)
!
!     NOTE - THE CURRENT CALL TO CPUTIM MUST GIVE A TIME VALUE BIGGER
!     THAN PREVIOUS CPUTIME CALL. OTHERWISE, CALLING ROUTINE MAY GET
!     INTO TROUBLE, SUCH AS DIVIDED BY ZERO.
!
!DME  19 JAN 2016
!DME  D. Everhart
!DME  Changed to conform to GFORTRAN implementation of ETIME subroutine.
   CALL etime(array,time)
!DME  CALL ETIME(ARRAY)
!DME
   t = array(2)
   IF ( Iflag/=0 ) THEN
      save = Rcpusc
      Rcpusc = t
      IF ( Rcpusc<=save ) Rcpusc = Rcpusc + 0.0001
   ELSE
      Icpusc = t + .49
   ENDIF
!
END SUBROUTINE cputim