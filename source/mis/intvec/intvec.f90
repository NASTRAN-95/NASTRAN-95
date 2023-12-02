!*==intvec.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE intvec(Vector)
   USE c_system
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Vector
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: char , i , k , nshape , vecwrd
   INTEGER , SAVE :: n
   INTEGER , DIMENSION(4) :: vec
   INTEGER , DIMENSION(4) , SAVE :: xyzr
   EXTERNAL klshft , krshft
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   DATA xyzr/1HX , 1HY , 1HZ , 1HR/
   DATA n/1HN/
!
   nshape = 0
   vecwrd = Vector
   IF ( vecwrd/=0 ) THEN
      DO i = 1 , 4
         vec(i) = 0
      ENDDO
!
!     SEPARATE THE FOUR CHARACTERS IN -VECWRD- (ANY COMBINATION OF THE
!     CHARACTERS X, Y, Z, AND R.
!
      DO k = 1 , 4
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               char = klshft(vecwrd,(k-1))
               char = krshft(char,(ncpw-1))
               DO i = 1 , 4
                  IF ( char==krshft(xyzr(i),(ncpw-1)) ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDDO
               IF ( char==krshft(n,(ncpw-1)) ) nshape = 1
            CASE (2)
               vec(i) = 1
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
!
      Vector = vec(1) + 2*vec(2) + 4*vec(3) + 8*vec(4)
      IF ( Vector==8 ) Vector = 15
      IF ( nshape==1 ) Vector = -Vector
   ENDIF
END SUBROUTINE intvec
