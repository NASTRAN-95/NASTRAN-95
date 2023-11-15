
FUNCTION mindeg(Nc,Ic,Ideg)
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
   INTEGER mindeg
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
!     THIS FUNCTION HAS AS ITS VALUE THE MINIMUM DEGREE OF ANY NODE OF
!     COMPONENT NC IF NC.GT.0
!     IF NC.LE.0, ALL COMPONENTS ARE CONSIDERED.
!
!
   m = 600000
   DO i = 1 , Nn
      IF ( Nc/=0 ) THEN
         IF ( Ic(i)/=Nc ) CYCLE
      ENDIF
      IF ( m>Ideg(i) ) m = Ideg(i)
   ENDDO
   mindeg = m
END FUNCTION mindeg
