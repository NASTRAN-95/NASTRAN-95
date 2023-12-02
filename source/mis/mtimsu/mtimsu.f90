!*==mtimsu.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE mtimsu(Y,X,Buf)
   IMPLICIT NONE
   USE c_invpwx
   USE c_invpxx
   USE c_names
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: Y
   REAL*8 , DIMENSION(1) :: X
   REAL , DIMENSION(1) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: da
   INTEGER :: i , ncol
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
!     M TIMS U  FORMS THE  PRODUCT  X = M*Y
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
   !>>>>EQUIVALENCE (A(1),Da)
!
!
   ncol = filek(2)
   DO i = 1 , ncol
      X(i) = 0.D0
   ENDDO
!
!     MASS MATRIX IS NOT DIAGONAL
!
   nzero = 0
   DO i = 1 , ncol
      IF ( Y(i)==0.D0 ) THEN
         CALL skprec(filem,1)
         nzero = nzero + 1
      ELSE
         CALL intpk(*100,filem(1),0,rdp,0)
         nzero = nzero + 1
         DO
            CALL zntpki
            X(ii) = da*Y(i) + X(ii)
            IF ( eol/=0 ) EXIT
         ENDDO
      ENDIF
 100  ENDDO
   CALL rewind(filem(1))
   CALL skprec(filem,1)
   nzero = ncol - nzero
END SUBROUTINE mtimsu
