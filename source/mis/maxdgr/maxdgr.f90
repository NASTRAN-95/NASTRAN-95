!*==maxdgr.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION maxdgr(Nc,Ic,Ideg)
   USE c_bands
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: maxdgr
   INTEGER :: Nc
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , m
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS FUNCTION HAS AS ITS VALUE THE MAXIMUM DEGREE OF ANY NODE OF
!     COMPONENT NC IF NC.GT.0
!     IF NC.LE.0, ALL COMPONENTS ARE CONSIDERED.
!
   m = 0
   DO i = 1 , nn
      IF ( Nc/=0 ) THEN
         IF ( Ic(i)/=Nc ) CYCLE
      ENDIF
      IF ( Ideg(i)>m ) m = Ideg(i)
   ENDDO
   maxdgr = m
END FUNCTION maxdgr
