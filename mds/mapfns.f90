
FUNCTION mapfns(I)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Lqro , M(3)
   COMMON /machin/ M , Lqro
!
! Dummy argument declarations
!
   INTEGER I , J
   INTEGER mapfns
!
! Local variable declarations
!
   INTEGER and , andf , complf , locfx , lshift , orf , rshift , xorf
   INTEGER k
   INTEGER loc
!
! End of declarations
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
   k = Lqro/1000
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
