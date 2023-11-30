
SUBROUTINE form22(Uddip1,Udiprm,Uiprm,Piprm,Ibuf)
   IMPLICIT NONE
   INTEGER Ifilb(7) , Ifilk(7) , Ifilm(7)
   COMMON /trdxx / Ifilk , Ifilm , Ifilb
   INTEGER Ibuf(1)
   DOUBLE PRECISION Piprm(1) , Uddip1(1) , Udiprm(1) , Uiprm(1)
!*******
!     FORM22 GENERATES THE VECTORS NECESSARY TO CHANGE THE TIME STEP
!
!     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
!*******
!
!
!*******
!     FORM UDOT(I+1), UDDOT(I+1), UDOT-(I), AND U-(I)
!*******
   CALL matvc2(Uddip1(1),Piprm(1),Ifilm(1),Ibuf)
   CALL matvc2(Udiprm(1),Piprm(1),Ifilb(1),Ibuf)
   CALL matvc2(Uiprm(1),Piprm(1),Ifilk(1),Ibuf)
END SUBROUTINE form22
