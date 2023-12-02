!*==sortdg.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sortdg(Stk1,Stk2,X1,X2,Ndeg)
   IMPLICIT NONE
   USE C_BANDG
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Stk1
   INTEGER , DIMENSION(1) :: Stk2
   INTEGER :: X1
   INTEGER :: X2
   INTEGER , DIMENSION(1) :: Ndeg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ind , istk2 , itest , j , jstk2 , temp
!
! End of declarations rewritten by SPAG
!
!
!     SORTDG SORTS STK2 BY DEGREE OF THE NODE AND ADDS IT TO THE END
!     OF STK1 IN ORDER OF LOWEST TO HIGHEST DEGREE.  X1 AND X2 ARE THE
!     NUMBER OF NODES IN STK1 AND STK2 RESPECTIVELY.
!
!
   ind = X2
   SPAG_Loop_1_1: DO
      itest = 0
      ind = ind - 1
      IF ( ind<1 ) EXIT SPAG_Loop_1_1
      DO i = 1 , ind
         j = i + 1
         istk2 = Stk2(i)
         jstk2 = Stk2(j)
         IF ( Ndeg(istk2)>Ndeg(jstk2) ) THEN
            itest = 1
            temp = Stk2(i)
            Stk2(i) = Stk2(j)
            Stk2(j) = temp
         ENDIF
      ENDDO
      IF ( itest/=1 ) EXIT SPAG_Loop_1_1
   ENDDO SPAG_Loop_1_1
   DO i = 1 , X2
      X1 = X1 + 1
      Stk1(X1) = Stk2(i)
   ENDDO
END SUBROUTINE sortdg
