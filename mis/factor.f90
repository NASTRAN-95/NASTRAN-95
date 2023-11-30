
SUBROUTINE factor(Input,Lower,Scr1,Scr2,Scr3,Scr4)
!
   IMPLICIT NONE
   INTEGER Chl , Filea(7) , Filel(7) , Fileu(7) , Ksystm(65) , Nz , P , Scr1fl , Scr2fl , Scr3fl , Xx3 , Xx4 , Z(1)
   DOUBLE PRECISION Det(2)
   COMMON /sfact / Filea , Filel , Fileu , Scr1fl , Scr2fl , Nz , Det , P , Scr3fl , Xx3 , Xx4 , Chl
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER Input , Lower , Scr1 , Scr2 , Scr3 , Scr4
   INTEGER bcd(2) , lowtri
   INTEGER korsz
   DATA lowtri/4/
   DATA bcd/4HFACT , 4HOR  /
!
!     INITIALIZE MATRIX CONTROL BLOCKS AND SFACT COMMON
!
   Nz = korsz(Z)
   Filea(1) = Input
   CALL rdtrl(Filea)
   CALL makmcb(Filel,Lower,Filea(3),lowtri,Filea(5))
   Fileu(1) = iabs(Scr1)
   Scr1fl = Scr2
   Scr2fl = Scr3
   Scr3fl = Scr4
   Chl = 0
   IF ( Scr1<0 ) Chl = 1
!
!     DECOMPOSE INPUT MATRIX INTO LOWER TRIANGULAR FACTOR.
!
   CALL sdcomp(*100,Z,Z,Z)
!
!     WRITE TRAILER FOR LOWER TRIANGULAR FACTOR.
!
   CALL wrttrl(Filel)
   RETURN
!
!     FATAL ERROR MESSAGE FOR SINGULAR INPUT MATRIX
!
 100  CALL mesage(-5,Input,bcd)
END SUBROUTINE factor
