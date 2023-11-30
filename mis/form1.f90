
SUBROUTINE form1(U0,Udot0,U1,P0,P1,Deltt,Ibuf)
   IMPLICIT NONE
   REAL Dummy(5)
   INTEGER Ifilb(7) , Ifilk(7) , Ifilm(7) , Istart
   COMMON /blank / Dummy , Istart
   COMMON /trdxx / Ifilk , Ifilm , Ifilb
   REAL Deltt
   INTEGER Ibuf(1)
   REAL P0(1) , P1(1) , U0(1) , U1(1) , Udot0(1)
   INTEGER i , nrow
!*******
!     FORM1 GENERATES THE STARTING VECTORS FOR THE INTEGRATION MODULE
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!*******
!
!
   nrow = Ifilk(2)
!
!*******
!     FORM U(-1)
!*******
   DO i = 1 , nrow
      P1(i) = 0.
      U1(i) = U0(i) - Deltt*Udot0(i)
   ENDDO
   IF ( Istart>=0 ) THEN
!
!     ALTERNATE STARTING METHOD
!
      CALL matvec(U0(1),P1(1),Ifilk(1),Ibuf)
      CALL matvec(Udot0(1),P1(1),Ifilb(1),Ibuf)
      DO i = 1 , nrow
         P0(i) = 0.5*(P0(i)+P1(i))
         Udot0(i) = -Udot0(i)*Deltt
      ENDDO
!
!     ADD UDOT CONTRIBUTION
!
      CALL matvec(Udot0(1),P1(1),Ifilk(1),Ibuf)
!
!     RESTORE UDOT
!
      DO i = 1 , nrow
         Udot0(i) = -Udot0(i)/Deltt
      ENDDO
      GOTO 99999
   ENDIF
   DO i = 1 , nrow
      P0(i) = 0.0
   ENDDO
!*******
!     FORM P0
!*******
   CALL matvec(U0(1),P0(1),Ifilk(1),Ibuf)
   CALL matvec(Udot0(1),P0(1),Ifilb(1),Ibuf)
!*******
!     FORM P(-1)
!*******
   CALL matvec(Udot0(1),P1(1),Ifilk(1),Ibuf)
   DO i = 1 , nrow
      P1(i) = P0(i) - Deltt*P1(i)
   ENDDO
   RETURN
99999 RETURN
END SUBROUTINE form1
