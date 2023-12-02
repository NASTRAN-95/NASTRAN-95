!*==magpha.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE magpha(A,B)
   IMPLICIT NONE
   USE C_CONDAS
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: A
   REAL :: B
!
! Local variable declarations rewritten by SPAG
!
   REAL :: phase , radeg , value
!
! End of declarations rewritten by SPAG
!
!*****
! THIS SUBROUTINE FORMS THE MAGNITUDE OF (A,B) AND STORES IT IN A...
! THE PHASE OF (X=A, Y=B) IS THEN FORMED AND THE RESULT STORED IN B...
!*****
!
   !>>>>EQUIVALENCE (Consts(3),Radeg)
!
   value = sqrt(A**2+B**2)
   IF ( value/=0 ) THEN
      phase = atan2(B,A)*radeg
      IF ( phase<(-0.00005E0) ) phase = phase + 360.0E0
   ELSE
      phase = 0.0E0
   ENDIF
   A = value
   B = phase
END SUBROUTINE magpha
