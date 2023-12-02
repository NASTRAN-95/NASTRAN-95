!*==tranp1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tranp1(In,Iout,Nscrth,Is1,Is2,Is3,Is4,Is5,Is6,Is7,Is8)
   IMPLICIT NONE
   USE C_TRNSPX
   USE C_ZZZZZZ
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
   Ia(1) = In
   CALL rdtrl(Ia)
   Iat(1) = Iout
   Iat(2) = Ia(3)
   Iat(3) = Ia(2)
   Iat(5) = Ia(5)
   Iat(4) = Ia(4)
!
!     REVERSE THE FORM OF THE LOWER AND UPPER TRIANGULAR MATRIX
!
   IF ( Ia(4)==4 ) Iat(4) = 5
   IF ( Ia(4)==5 ) Iat(4) = 4
   Lcore = korsz(Core)
   Nscrh = Nscrth
   Scr(1) = Is1
   Scr(2) = Is2
   Scr(3) = Is3
   Scr(4) = Is4
   Scr(5) = Is5
   Scr(6) = Is6
   Scr(7) = Is7
   Scr(8) = Is8
   CALL trnsp(Core)
   CALL wrttrl(Iat)
END SUBROUTINE tranp1
