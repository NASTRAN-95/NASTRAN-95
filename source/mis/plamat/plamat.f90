!*==plamat.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE plamat
   IMPLICIT NONE
   USE C_MATIN
   USE C_MATOUT
   USE C_PLAGP
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
   IF ( Midgp/=Matid ) THEN
      Inflag = 2
      CALL mat(Elid)
      RETURN
   ENDIF
!
!                           T
!  TRANSFORM G   ,  G  =   U  *  G   * U
!             P      P            P
!
   x(1) = Costh**2
   x(2) = Sinth**2
   x(3) = Costh*Sinth
   x(4) = x(2)
   x(5) = x(1)
   x(6) = -x(3)
   x(7) = 2.0*x(6)
   x(8) = -x(7)
   x(9) = x(1) - x(2)
   CALL gmmats(Gp(1),3,3,0,x(1),3,3,0,x(19))
   CALL gmmats(x(1),3,3,1,x(19),3,3,0,x(10))
   G11 = x(10)
   G12 = x(11)
   G13 = x(12)
   G22 = x(14)
   G23 = x(15)
   G33 = x(18)
   RETURN
END SUBROUTINE plamat
