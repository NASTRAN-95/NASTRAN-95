!*==a82int.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE a82int(*,A,N,B,Int)
   USE c_xreadx
   USE C_XREADX
   IMPLICIT NONE
   INTEGER Nout
   COMMON /xreadx/ Nout
   REAL B
   CHARACTER*8 C
   INTEGER Int , N
   REAL A(2)
   INTEGER nt
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY k82int(*,C,N,B,Int)
!     ****************************
!
         nt = +1
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
!
         ENTRY a82fp(*,A,N,B,Int)
!     ***************************
!
         nt = -1
         spag_nextblock_1 = 2
      CASE (2)
!
         IF ( N>8 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Int = nt
         CALL na12if(*20,A,N,B,Int)
         RETURN
!
         ENTRY k82fp(*,C,N,B,Int)
!     ***************************
!
         nt = -1
         spag_nextblock_1 = 3
      CASE (3)
!
         IF ( N<=8 ) THEN
            Int = nt
            CALL nk12if(*20,C,N,B,Int)
            RETURN
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
!
         WRITE (Nout,99001) N , nt
99001    FORMAT ('  N.GT.8/A82INT',I5,7X,'NT=',I2)
 20      RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE a82int
