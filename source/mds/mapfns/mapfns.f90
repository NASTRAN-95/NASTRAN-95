!*==mapfns.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION mapfns(I)
   USE c_machin
   IMPLICIT NONE
   INTEGER :: mapfns
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: I
   *0() :: 
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: and , andf , complf , locfx , lshift , orf , rshift , xorf
   INTEGER :: J
   INTEGER :: k
!
! End of declarations rewritten by SPAG
!
!
!     THIS FUNCTION PROVIDES ENTRIES FOR VARIOUS FUNCTIONS
!     ON THE VAX VERSION OF NASTRAN
!     (THIS ROUTINE WAS PREVIOUSLY CALLED 'VAXFNS')
!
!
   mapfns = 0
   RETURN
!
   ENTRY and(I,J)
!     ==============
   and = iand(I,J)
   RETURN
!
   ENTRY andf(I,J)
!     ================
   andf = iand(I,J)
   RETURN
!
   ENTRY complf(I)
!     ================
   complf = not(I)
   RETURN
!
   ENTRY locfx(I)
!     ===============
   k = lqro/1000
   locfx = loc(I)/k
   RETURN
!
   ENTRY lshift(I,J)
!     ==================
   lshift = ishft(I,J)
   RETURN
!
   ENTRY orf(I,J)
!     ===============
   orf = ior(I,J)
   RETURN
!
   ENTRY rshift(I,J)
!     ==================
   rshift = ishft(I,-J)
   RETURN
!
   ENTRY xorf(I,J)
!     ================
   xorf = ieor(I,J)
!
END FUNCTION mapfns
