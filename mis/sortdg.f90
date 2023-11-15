
SUBROUTINE sortdg(Stk1,Stk2,X1,X2,Ndeg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Idpth , N
   COMMON /bandg / N , Idpth
!
! Dummy argument declarations
!
   INTEGER X1 , X2
   INTEGER Ndeg(1) , Stk1(1) , Stk2(1)
!
! Local variable declarations
!
   INTEGER i , ind , istk2 , itest , j , jstk2 , temp
!
! End of declarations
!
!
!     SORTDG SORTS STK2 BY DEGREE OF THE NODE AND ADDS IT TO THE END
!     OF STK1 IN ORDER OF LOWEST TO HIGHEST DEGREE.  X1 AND X2 ARE THE
!     NUMBER OF NODES IN STK1 AND STK2 RESPECTIVELY.
!
!
   ind = X2
   DO
      itest = 0
      ind = ind - 1
      IF ( ind<1 ) EXIT
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
      IF ( itest/=1 ) EXIT
   ENDDO
   DO i = 1 , X2
      X1 = X1 + 1
      Stk1(X1) = Stk2(i)
   ENDDO
END SUBROUTINE sortdg
