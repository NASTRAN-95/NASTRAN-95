!*==mtmsu1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mtmsu1(Y,X,Buf)
   IMPLICIT NONE
   USE C_INVPWX
   USE C_INVPXX
   USE C_NAMES
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Y
   REAL , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   REAL :: da
   INTEGER :: i , ncol
!
! End of declarations rewritten by SPAG
!
!
!     M TIMS U  FORMS THE PRODUCT  X = M*Y
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   !>>>>EQUIVALENCE (A(1),Da)
!
!
   ncol = Filek(2)
   DO i = 1 , ncol
      X(i) = 0.0
   ENDDO
!
!     MASS MATRIX IS NOT DIAGONAL
!
   Nzero = 0
   DO i = 1 , ncol
      IF ( Y(i)==0.0 ) THEN
         CALL skprec(Filem,1)
         Nzero = Nzero + 1
      ELSE
         CALL intpk(*100,Filem(1),0,Rsp,0)
         Nzero = Nzero + 1
         DO
            CALL zntpki
            X(Ii) = da*Y(i) + X(Ii)
            IF ( Eol/=0 ) EXIT
         ENDDO
      ENDIF
 100  ENDDO
   CALL rewind(Filem(1))
   CALL skprec(Filem,1)
   Nzero = ncol - Nzero
END SUBROUTINE mtmsu1
