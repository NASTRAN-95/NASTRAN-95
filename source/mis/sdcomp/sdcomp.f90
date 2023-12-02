!*==sdcomp.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdcomp(Zi,Zr,Zd) !HIDESTARS (*,Zi,Zr,Zd)
   USE i_smcomx
   USE c_logout
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: Zi
   REAL :: Zr
   REAL :: Zd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i44
!
! End of declarations rewritten by SPAG
!
   CALL sswtch(44,i44)
   IF ( i44/=0 ) THEN
!
! OTHERWISE, CALL SYMMETRIC DECOMPOSITION OF RELEASE 94 AND EARLIER
!
      CALL sdcompx(*100,Zi,Zr,Zd)
   ELSE
!
! CALL NEW SYMMETRIC DECOMPOSITION ROUTINE 12/95
!
      CALL smcomp(*100,Zi,Zr,Zd)
      IF ( ierror==1 ) THEN
         WRITE (lout,99001)
99001    FORMAT (8X,'INSUFFICIENT OPEN CORE FOR NEW SYMMETRIC DECOMPOSITION',/,8X,'WILL SWITCH AND USE OLD METHOD.')
         CALL sdcompx(*100,Zi,Zr,Zd)
      ENDIF
   ENDIF
   RETURN
 100  RETURN 1
END SUBROUTINE sdcomp
