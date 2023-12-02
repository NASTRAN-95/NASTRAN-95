!*==tranp1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tranp1(In,Iout,Nscrth,Is1,Is2,Is3,Is4,Is5,Is6,Is7,Is8)
   USE c_trnspx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: In
   INTEGER :: Iout
   INTEGER :: Nscrth
   INTEGER :: Is1
   INTEGER :: Is2
   INTEGER :: Is3
   INTEGER :: Is4
   INTEGER :: Is5
   INTEGER :: Is6
   INTEGER :: Is7
   INTEGER :: Is8
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL korsz , mesage , rdtrl , trnsp , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     DRIVER OF THE OUT-OF-CORE MATRIX TRANSPOSE ROUTINE TRNSP
!     (DTRANP IS THE TRNSP MODULE DRIVER)
!
!
   DATA nam/4HTRNS , 4HP1  /
!
   IF ( Nscrth>8 ) CALL mesage(-37,0,nam)
   ia(1) = In
   CALL rdtrl(ia)
   iat(1) = Iout
   iat(2) = ia(3)
   iat(3) = ia(2)
   iat(5) = ia(5)
   iat(4) = ia(4)
!
!     REVERSE THE FORM OF THE LOWER AND UPPER TRIANGULAR MATRIX
!
   IF ( ia(4)==4 ) iat(4) = 5
   IF ( ia(4)==5 ) iat(4) = 4
   lcore = korsz(core)
   nscrh = Nscrth
   scr(1) = Is1
   scr(2) = Is2
   scr(3) = Is3
   scr(4) = Is4
   scr(5) = Is5
   scr(6) = Is6
   scr(7) = Is7
   scr(8) = Is8
   CALL trnsp(core)
   CALL wrttrl(iat)
END SUBROUTINE tranp1
