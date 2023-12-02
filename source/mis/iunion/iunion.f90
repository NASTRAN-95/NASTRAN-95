!*==iunion.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION iunion(I1,I2)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: iunion
   INTEGER :: I1
   INTEGER :: I2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ii , j , l , r , spag_nextblock_1
   INTEGER , DIMENSION(6,2) :: k
   INTEGER , DIMENSION(6) :: kk
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     I1 AND I2 ARE .GT. 0 BUT .LE. 654321 AND CONSIST OF ANY UNIQUE
!     COMBINATION OF THE DIGITS 1 THRU 6
!
!
!              DECODE I1 INTO K(*,1)
         i = 1
         ii = I1
         ASSIGN 20 TO r
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!              DECODE I2 INTO K(*,2)
 20      i = 2
         ii = I2
         ASSIGN 40 TO r
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!              FORM UNION OF K(*,1) AND K(*,2) IN KK(*)
 40      DO i = 1 , 6
            kk(i) = 0
            IF ( k(i,1)==i .OR. k(i,2)==i ) kk(i) = i
         ENDDO
!
!              PACK KK(*) INTO IUNION
         j = 1
         l = 0
         DO i = 1 , 6
            IF ( kk(i)/=0 ) THEN
               IF ( l>0 ) j = 10*j
               l = l + j*i
            ENDIF
         ENDDO
!
         iunion = l
!
         RETURN
      CASE (2)
!
!
         DO j = 1 , 6
            k(j,i) = 0
         ENDDO
         SPAG_Loop_1_1: DO j = 1 , 6
            l = ii - 10*(ii/10)
            ii = (ii-l)/10
            IF ( l==0 ) EXIT SPAG_Loop_1_1
            k(l,i) = l
         ENDDO SPAG_Loop_1_1
         GOTO r
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END FUNCTION iunion
