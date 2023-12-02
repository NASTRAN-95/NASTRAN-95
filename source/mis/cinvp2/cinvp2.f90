!*==cinvp2.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cinvp2() !HIDESTARS (*)
   USE c_cdcmpx
   USE c_cinvpx
   USE c_cinvxx
   USE c_names
   USE c_system
   USE c_zzzzzz
   USE iso_fortran_env
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
   ioff = fileu(7)
   filea(1) = scr1
   IF ( switch==0 ) THEN
      filel(1) = scr3
      fileu(1) = scr4
   ELSE
      filel(1) = scr8
      fileu(1) = scr9
      IF ( switch<0 ) filea(1) = -filea(1)
      IF ( switch/=-204 ) switch = 1
   ENDIF
   sr1fil = scr5
   sr2fil = scr6
   sr3fil = scr7
   filea(2) = dumm(3)
   filea(3) = dumm(3)
   filea(4) = dumm(4)
   filea(5) = cdp
   filea(6) = 0
   filea(7) = 0
   filel(5) = cdp
   nz = korsz(z)
   IF ( switch==-204 ) nz = nz - 2*sysbuf
   ib = 0
   CALL cdcomp(*100,z,z,z)
   IF ( switch/=0 ) fileu(7) = ioff
   RETURN
!
 100  RETURN 1
END SUBROUTINE cinvp2
