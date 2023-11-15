
SUBROUTINE a82int(*,A,N,B,Int)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Nout
   COMMON /xreadx/ Nout
!
! Dummy argument declarations
!
   REAL B
   CHARACTER*8 C
   INTEGER Int , N
   REAL A(2)
!
! Local variable declarations
!
   INTEGER nt
!
! End of declarations
!
!
!
!     THESE ROUTINES PERFORM IN THE OPPOSITE DIRECTION AS THOSE OF THE
!     INT2A8 GROUP OF ROUTINES
!     THIS ROUTINE IS MACHINE INDEPENDENT
!
!     ENTRY POINTS   A8 2 INT  (BCD-INTEGER VERSION)
!                    K8 2 INT  (CHARACTER-INTEGER VERSION)
!                    A8 2 FP   (BCD-REAL VERSION)
!                    K8 2 FP   (CHARACTER-REAL VERSION)
!
   nt = +1
   GOTO 100
!
   ENTRY k82int(*,C,N,B,Int)
!     ****************************
!
   nt = +1
   GOTO 200
!
   ENTRY a82fp(*,A,N,B,Int)
!     ***************************
!
   nt = -1
!
 100  IF ( N>8 ) GOTO 300
   Int = nt
   CALL na12if(*400,A,N,B,Int)
   RETURN
!
   ENTRY k82fp(*,C,N,B,Int)
!     ***************************
!
   nt = -1
!
 200  IF ( N<=8 ) THEN
      Int = nt
      CALL nk12if(*400,C,N,B,Int)
      RETURN
   ENDIF
!
 300  WRITE (Nout,99001) N , nt
99001 FORMAT ('  N.GT.8/A82INT',I5,7X,'NT=',I2)
 400  RETURN 1
END SUBROUTINE a82int
