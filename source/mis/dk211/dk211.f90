!*==dk211.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION dk211(I,A,B,X)
USE iso_fortran_env
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) :: dk211
   INTEGER :: I
   REAL(REAL64) :: A
   REAL(REAL64) :: B
   REAL(REAL64) , DIMENSION(1) :: X
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: aaj , c1 , c2 , c3 , f6211 , xx
   INTEGER :: j
!
! End of declarations rewritten by SPAG
!
!
! Function and Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
   xx = X(I)
   IF ( (B*xx)**2<A**2 ) THEN
      f6211 = dlog(dabs(A))*dlog(dabs(xx))
      c1 = -B*xx/A
      c2 = 1.0D0
      j = 0
      DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 - c3
         IF ( dabs(c3)<=0.1D-5 ) THEN
            dk211 = f6211
            RETURN
         ENDIF
      ENDDO
   ELSEIF ( (B*xx)**2==A**2 ) THEN
      IF ( A/=B*xx ) THEN
         f6211 = 0.0D0
         dk211 = f6211
         RETURN
      ELSE
         f6211 = 0.5D0*(dlog(dabs(2.0D0*B*xx)))**2
         dk211 = f6211
         RETURN
      ENDIF
   ELSE
      f6211 = (dlog(dabs(B*xx))**2)/2.0D0
      c1 = -A/(B*xx)
      c2 = 1.0D0
      j = 0
      SPAG_Loop_1_1: DO
         j = j + 1
         aaj = j
         c2 = c2*c1
         c3 = c2/(aaj**2)
         f6211 = f6211 + c3
         IF ( dabs(c3)<=0.1D-5 ) THEN
            dk211 = f6211
            EXIT SPAG_Loop_1_1
         ENDIF
      ENDDO SPAG_Loop_1_1
   ENDIF
END FUNCTION dk211
