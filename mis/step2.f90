
SUBROUTINE step2(U2,U1,U0,P,Ibuf)
   IMPLICIT NONE
   INTEGER Dum(21) , Ifil(7) , Ifilu(7) , Iopen , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Isym , Rdp , Sqr
   REAL Dumm(8) , Dumn(2)
   COMMON /infbsx/ Ifil , Ifilu
   COMMON /names / Dumm , Rdp , Dumn , Sqr
   COMMON /trdxx / Dum , Iscr1 , Iscr2 , Iscr3 , Iscr4 , Iscr5 , Iscr6 , Iopen , Isym
   INTEGER Ibuf(1)
   DOUBLE PRECISION P(1) , U0(1) , U1(1) , U2(1)
   INTEGER file(7)
!
!     STEP2 WILL INTEGRATE FORWARD ONE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!
!
   file(1) = Iscr1
   file(2) = Dum(2)
   file(4) = Sqr
!
!     TELL MATVC2/INVFBS FILES ARE OPEN
!
   Iopen = 1
!
!     FORM R.H.S. OF THE INTEGRATION EQUATION
!
   CALL matvc2(U1(1),P(1),file,Ibuf)
   file(1) = Iscr4
   CALL matvc2(U0(1),P(1),file,Ibuf)
!
!     CALL INVFBS/FBSINT TO DO THE FORWARD/BACKWARD PASS
!
   Ifil(1) = Iscr2
   Ifilu(1) = Iscr3
   CALL rdtrl(Ifil)
   CALL rdtrl(Ifilu)
   Ifil(5) = Rdp
   Ifil(3) = Dum(3)
   IF ( Isym==1 ) Iopen = -20
   IF ( Isym==1 ) CALL invfbs(P(1),U2(1),Ibuf)
   IF ( Isym==0 ) CALL fbsint(P(1),U2(1))
   Iopen = 0
END SUBROUTINE step2