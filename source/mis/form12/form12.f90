!*==form12.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE form12(U0,Udot0,U1,P0,P1,Deltt,Ibuf)
USE C_BLANK
USE C_TRDXX
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: U0
   REAL(REAL64) , DIMENSION(1) :: Udot0
   REAL(REAL64) , DIMENSION(1) :: U1
   REAL(REAL64) , DIMENSION(1) :: P0
   REAL(REAL64) , DIMENSION(1) :: P1
   REAL :: Deltt
   INTEGER , DIMENSION(1) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nrow
   EXTERNAL matvc2
!
! End of declarations rewritten by SPAG
!
!*******
!     FORM12 GENERATES THE STARTING VECTORS FOR THE INTEGRATION MODULE
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!*******
!
!
!
   nrow = Ifilk(2)
!*******
!     FORM U(-1)
!*******
   DO i = 1 , nrow
      P1(i) = 0.0D0
      U1(i) = U0(i) - Deltt*Udot0(i)
   ENDDO
   IF ( Istart>=0 ) THEN
!
!     ALTERNATE STARTING METHOD
!
      CALL matvc2(U0(1),P1(1),Ifilk(1),Ibuf)
      CALL matvc2(Udot0(1),P1(1),Ifilb(1),Ibuf)
      DO i = 1 , nrow
         P0(i) = 0.5D0*(P0(i)+P1(i))
         Udot0(i) = -Udot0(i)*Deltt
      ENDDO
!
!     ADD UDOT CONTRIBUTION
!
      CALL matvc2(Udot0(1),P1(1),Ifilk(1),Ibuf)
!
!     RESTORE UDOT
!
      DO i = 1 , nrow
         Udot0(i) = -Udot0(i)/Deltt
      ENDDO
      RETURN
   ENDIF
   DO i = 1 , nrow
      P0(i) = 0.0D0
   ENDDO
!*******
!     FORM P0
!*******
   CALL matvc2(U0(1),P0(1),Ifilk(1),Ibuf)
   CALL matvc2(Udot0(1),P0(1),Ifilb(1),Ibuf)
!*******
!     FORM P(-1)
!*******
   CALL matvc2(Udot0(1),P1(1),Ifilk(1),Ibuf)
   DO i = 1 , nrow
      P1(i) = P0(i) - Deltt*P1(i)
   ENDDO
   RETURN
END SUBROUTINE form12
