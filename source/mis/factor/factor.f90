!*==factor.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE factor(Input,Lower,Scr1,Scr2,Scr3,Scr4)
!
   IMPLICIT NONE
   USE c_sfact
   USE c_system
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Input
   INTEGER :: Lower
   INTEGER :: Scr1
   INTEGER :: Scr2
   INTEGER :: Scr3
   INTEGER :: Scr4
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: bcd
   INTEGER , SAVE :: lowtri
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
   DATA lowtri/4/
   DATA bcd/4HFACT , 4HOR  /
!
!     INITIALIZE MATRIX CONTROL BLOCKS AND SFACT COMMON
!
   nz = korsz(z)
   filea(1) = Input
   CALL rdtrl(filea)
   CALL makmcb(filel,Lower,filea(3),lowtri,filea(5))
   fileu(1) = iabs(Scr1)
   scr1fl = Scr2
   scr2fl = Scr3
   scr3fl = Scr4
   chl = 0
   IF ( Scr1<0 ) chl = 1
!
!     DECOMPOSE INPUT MATRIX INTO LOWER TRIANGULAR FACTOR.
!
   CALL sdcomp(*100,z,z,z)
!
!     WRITE TRAILER FOR LOWER TRIANGULAR FACTOR.
!
   CALL wrttrl(filel)
   RETURN
!
!     FATAL ERROR MESSAGE FOR SINGULAR INPUT MATRIX
!
 100  CALL mesage(-5,Input,bcd)
END SUBROUTINE factor
