
SUBROUTINE step(U2,U1,U0,P,Ibuf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dum(21) , Ifil(7) , Ifilu(7) , Iopen , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Isym , Rsp , Sqr
   REAL Dumm(7) , Dumn(3)
   COMMON /infbsx/ Ifil , Ifilu
   COMMON /names / Dumm , Rsp , Dumn , Sqr
   COMMON /trdxx / Dum , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iopen , Isym
!
! Dummy argument declarations
!
   INTEGER Ibuf(1)
   REAL P(1) , U0(1) , U1(1) , U2(1)
!
! Local variable declarations
!
   INTEGER file(7)
!
! End of declarations
!
!
!     STEP WILL INTEGRATE FORWARD ONE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!
!
   file(1) = Iscr1
   file(2) = Dum(2)
   file(4) = Sqr
!
!     TELL MATVEC/INTFBS FILES ARE OPEN
!
   Iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvec(U1(1),P(1),file,Ibuf)
   file(1) = Iscr4
   CALL matvec(U0(1),P(1),file,Ibuf)
!
!     CALL INTFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   Ifil(1) = Iscr2
   Ifilu(1) = Iscr3
   CALL rdtrl(Ifil)
   CALL rdtrl(Ifilu)
   Ifil(5) = Rsp
   Ifil(3) = Dum(2)
   IF ( Isym==1 ) CALL intfbs(P(1),U2(1),Ibuf)
   IF ( Isym==0 ) CALL fbsint(P(1),U2(1))
   Iopen = 0
END SUBROUTINE step
