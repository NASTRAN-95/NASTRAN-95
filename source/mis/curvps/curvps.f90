!*==curvps.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE curvps(Sigs,Prin)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3) :: Sigs
   REAL , DIMENSION(4) :: Prin
!
! Local variable declarations rewritten by SPAG
!
   REAL :: delta , temp
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
!  COMPUTES PRINCIPAL STRESSES OR STRAINS AND ANGLE OF MAXIMUM.
!*****
!
   temp = Sigs(1) - Sigs(2)
   Prin(4) = sqrt((temp/2.0)**2+Sigs(3)**2)
   delta = (Sigs(1)+Sigs(2))/2.0
   Prin(2) = delta + Prin(4)
   Prin(3) = delta - Prin(4)
   delta = 2.0*Sigs(3)
   IF ( abs(delta)<1.0E-15 .AND. abs(temp)<1.0E-15 ) THEN
!
      Prin(1) = 0.0
      RETURN
   ENDIF
   Prin(1) = atan2(delta,temp)*28.6478898E0
END SUBROUTINE curvps
