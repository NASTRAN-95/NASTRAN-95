
SUBROUTINE magpha(A,B)
   IMPLICIT NONE
   REAL Consts(5) , Radeg
   COMMON /condas/ Consts
   REAL A , B
   REAL phase , value
!*****
! THIS SUBROUTINE FORMS THE MAGNITUDE OF (A,B) AND STORES IT IN A...
! THE PHASE OF (X=A, Y=B) IS THEN FORMED AND THE RESULT STORED IN B...
!*****
!
   EQUIVALENCE (Consts(3),Radeg)
!
   value = sqrt(A**2+B**2)
   IF ( value/=0 ) THEN
      phase = atan2(B,A)*Radeg
      IF ( phase<(-0.00005E0) ) phase = phase + 360.0E0
   ELSE
      phase = 0.0E0
   ENDIF
   A = value
   B = phase
END SUBROUTINE magpha
