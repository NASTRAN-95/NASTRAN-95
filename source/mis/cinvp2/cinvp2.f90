!*==cinvp2.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvp2() !HIDESTARS (*)
USE C_CDCMPX
USE C_CINVPX
USE C_CINVXX
USE C_NAMES
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ioff
   EXTERNAL cdcomp , korsz
!
! End of declarations rewritten by SPAG
!
!
!     CINVP2 INITIALIZES AND CALLS CDCOMP FOR CINVPR
!
!
   ioff = Fileu(7)
   Filea(1) = Scr1
   IF ( Switch==0 ) THEN
      Filel(1) = Scr3
      Fileu(1) = Scr4
   ELSE
      Filel(1) = Scr8
      Fileu(1) = Scr9
      IF ( Switch<0 ) Filea(1) = -Filea(1)
      IF ( Switch/=-204 ) Switch = 1
   ENDIF
   Sr1fil = Scr5
   Sr2fil = Scr6
   Sr3fil = Scr7
   Filea(2) = Dumm(3)
   Filea(3) = Dumm(3)
   Filea(4) = Dumm(4)
   Filea(5) = Cdp
   Filea(6) = 0
   Filea(7) = 0
   Filel(5) = Cdp
   Nz = korsz(Z)
   IF ( Switch==-204 ) Nz = Nz - 2*Sysbuf
   Ib = 0
   CALL cdcomp(*100,Z,Z,Z)
   IF ( Switch/=0 ) Fileu(7) = ioff
   RETURN
!
 100  RETURN 1
END SUBROUTINE cinvp2
