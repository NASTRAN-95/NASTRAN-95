
SUBROUTINE sdcomp(*,Zi,Zr,Zd)
   IMPLICIT NONE
   INCLUDE 'SMCOMX.COM'
!
! COMMON variable declarations
!
   INTEGER Lout
   COMMON /logout/ Lout
!
! Dummy argument declarations
!
   REAL Zd , Zi , Zr
!
! Local variable declarations
!
   INTEGER i44
!
! End of declarations
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
      IF ( Ierror==1 ) THEN
         WRITE (Lout,99001)
99001    FORMAT (8X,'INSUFFICIENT OPEN CORE FOR NEW SYMMETRIC DECOMPOSITION',/,8X,'WILL SWITCH AND USE OLD METHOD.')
         CALL sdcompx(*100,Zi,Zr,Zd)
      ENDIF
   ENDIF
   RETURN
 100  RETURN 1
END SUBROUTINE sdcomp
