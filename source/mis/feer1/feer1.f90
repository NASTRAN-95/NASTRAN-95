!*==feer1.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer1
USE C_FEERCX
USE C_FEERXX
USE C_NAMES
USE C_SADDX
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , iprec
   EXTERNAL korsz , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
!
!     FEER1 INITIALIZES AND CALLS SUBROUTINE ADD FOR FEER
!
   !>>>>EQUIVALENCE (Ksystm(55),Iprec)
!
!     SET UP CALL TO ADD
!
   DO i = 1 , 7
      Filea(i) = Filem(i)
      Fileb(i) = Filek(i)
   ENDDO
   Dalpha(1) = Lambda
   Dbeta(1) = 1.0D+0
   Typalp = iprec
   Typbta = iprec
   Nz = korsz(Z)
   Filec(1) = Scr1
   Filec(2) = Filek(2)
   Filec(3) = Filek(3)
   Filec(4) = Sqr
   Filec(5) = iprec
   Nomat = 2
   IF ( Fileb(1)==0 ) Nomat = 1
   CALL sadd(Z,Z)
   CALL wrttrl(Filec)
END SUBROUTINE feer1
