!*==area.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION area(G,I,J,K)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: area
   REAL , DIMENSION(1) :: G
   INTEGER :: I
   INTEGER :: J
   INTEGER :: K
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE IS CALLED BY SFAREA WHICH IS CALLED BY EMGFIN TO
!     COMPUTE THE SURFACE AREAS OF THE SOLID ELEMENTS
!
   area = 0.5*sqrt(((G(J+2)-G(I+2))*(G(K+3)-G(I+3))-(G(J+3)-G(I+3))*(G(K+2)-G(I+2)))                                                &
        & **2+((G(J+3)-G(I+3))*(G(K+1)-G(I+1))-(G(J+1)-G(I+1))*(G(K+3)-G(I+3)))**2+((G(J+1)-G(I+1))*(G(K+2)-G(I+2))-(G(J+2)-G(I+2)) &
        & *(G(K+1)-G(I+1)))**2)
END FUNCTION area
