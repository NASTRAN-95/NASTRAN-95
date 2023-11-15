
SUBROUTINE tranp1(In,Iout,Nscrth,Is1,Is2,Is3,Is4,Is5,Is6,Is7,Is8)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Core(1)
   INTEGER Ia(7) , Iat(7) , Lcore , Nscrh , Scr(8)
   COMMON /trnspx/ Ia , Iat , Lcore , Nscrh , Scr
   COMMON /zzzzzz/ Core
!
! Dummy argument declarations
!
   INTEGER In , Iout , Is1 , Is2 , Is3 , Is4 , Is5 , Is6 , Is7 , Is8 , Nscrth
!
! Local variable declarations
!
   INTEGER korsz
   INTEGER nam(2)
!
! End of declarations
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
