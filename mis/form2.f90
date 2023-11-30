
SUBROUTINE form2(Uddip1,Udiprm,Uiprm,Piprm,Ibuf)
   IMPLICIT NONE
   INTEGER Ifilb(7) , Ifilk(7) , Ifilm(7)
   COMMON /trdxx / Ifilk , Ifilm , Ifilb
   INTEGER Ibuf(1)
   REAL Piprm(1) , Uddip1(1) , Udiprm(1) , Uiprm(1)
!*******
!     FORM2 GENERATES THE VECTORS NECESSARY TO CHANGE THE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR SINGLE PRECISION OPERATION
!*******
!
!*******
!     FORM UDOT(I+1), UDDOT(I+1), UDOT-(I), AND U-(I)
!*******
   CALL matvec(Uddip1(1),Piprm(1),Ifilm(1),Ibuf)
   CALL matvec(Udiprm(1),Piprm(1),Ifilb(1),Ibuf)
   CALL matvec(Uiprm(1),Piprm(1),Ifilk(1),Ibuf)
END SUBROUTINE form2
