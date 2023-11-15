
FUNCTION maxdgr(Nc,Ic,Ideg)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Nn
   COMMON /bands / Nn
!
! Dummy argument declarations
!
   INTEGER Nc
   INTEGER Ic(1) , Ideg(1)
   INTEGER maxdgr
!
! Local variable declarations
!
   INTEGER i , m
!
! End of declarations
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS FUNCTION HAS AS ITS VALUE THE MAXIMUM DEGREE OF ANY NODE OF
!     COMPONENT NC IF NC.GT.0
!     IF NC.LE.0, ALL COMPONENTS ARE CONSIDERED.
!
   m = 0
   DO i = 1 , Nn
      IF ( Nc/=0 ) THEN
         IF ( Ic(i)/=Nc ) CYCLE
      ENDIF
      IF ( Ideg(i)>m ) m = Ideg(i)
   ENDDO
   maxdgr = m
END FUNCTION maxdgr
