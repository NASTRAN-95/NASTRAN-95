!*==plamat.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plamat
   USE c_matin
   USE c_matout
   USE c_plagp
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(27) :: x
   EXTERNAL gmmats , mat
!
! End of declarations rewritten by SPAG
!
! THIS ROUTINE RETURNS GP ROTATED FOR PLA3 AND PLA4
!
!
!  TEST TO SEE IF INCOMING MATERIAL ID IS EQUAL TO MATERIAL ID IN
!  PLAGP.  IF NOT USE REGULAR CALL TO MAT TO GET GP
!
   IF ( midgp/=matid ) THEN
      inflag = 2
      CALL mat(elid)
      RETURN
   ENDIF
!
!                           T
!  TRANSFORM G   ,  G  =   U  *  G   * U
!             P      P            P
!
   x(1) = costh**2
   x(2) = sinth**2
   x(3) = costh*sinth
   x(4) = x(2)
   x(5) = x(1)
   x(6) = -x(3)
   x(7) = 2.0*x(6)
   x(8) = -x(7)
   x(9) = x(1) - x(2)
   CALL gmmats(gp(1),3,3,0,x(1),3,3,0,x(19))
   CALL gmmats(x(1),3,3,1,x(19),3,3,0,x(10))
   g11 = x(10)
   g12 = x(11)
   g13 = x(12)
   g22 = x(14)
   g23 = x(15)
   g33 = x(18)
END SUBROUTINE plamat