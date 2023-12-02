!*==form1.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE form1(U0,Udot0,U1,P0,P1,Deltt,Ibuf)
   USE c_blank
   USE c_trdxx
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: U0
   REAL , DIMENSION(1) :: Udot0
   REAL , DIMENSION(1) :: U1
   REAL , DIMENSION(1) :: P0
   REAL , DIMENSION(1) :: P1
   REAL :: Deltt
   INTEGER , DIMENSION(1) :: Ibuf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nrow
   EXTERNAL matvec
!
! End of declarations rewritten by SPAG
!
!*******
!     FORM1 GENERATES THE STARTING VECTORS FOR THE INTEGRATION MODULE
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!*******
!
!
   nrow = ifilk(2)
!
!*******
!     FORM U(-1)
!*******
   DO i = 1 , nrow
      P1(i) = 0.
      U1(i) = U0(i) - Deltt*Udot0(i)
   ENDDO
   IF ( istart>=0 ) THEN
!
!     ALTERNATE STARTING METHOD
!
      CALL matvec(U0(1),P1(1),ifilk(1),Ibuf)
      CALL matvec(Udot0(1),P1(1),ifilb(1),Ibuf)
      DO i = 1 , nrow
         P0(i) = 0.5*(P0(i)+P1(i))
         Udot0(i) = -Udot0(i)*Deltt
      ENDDO
!
!     ADD UDOT CONTRIBUTION
!
      CALL matvec(Udot0(1),P1(1),ifilk(1),Ibuf)
!
!     RESTORE UDOT
!
      DO i = 1 , nrow
         Udot0(i) = -Udot0(i)/Deltt
      ENDDO
      RETURN
   ENDIF
   DO i = 1 , nrow
      P0(i) = 0.0
   ENDDO
!*******
!     FORM P0
!*******
   CALL matvec(U0(1),P0(1),ifilk(1),Ibuf)
   CALL matvec(Udot0(1),P0(1),ifilb(1),Ibuf)
!*******
!     FORM P(-1)
!*******
   CALL matvec(Udot0(1),P1(1),ifilk(1),Ibuf)
   DO i = 1 , nrow
      P1(i) = P0(i) - Deltt*P1(i)
   ENDDO
END SUBROUTINE form1
