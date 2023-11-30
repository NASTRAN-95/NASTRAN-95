
FUNCTION area(G,I,J,K)
   IMPLICIT NONE
   INTEGER I , J , K
   REAL area
   REAL G(1)
!
!     THIS ROUTINE IS CALLED BY SFAREA WHICH IS CALLED BY EMGFIN TO
!     COMPUTE THE SURFACE AREAS OF THE SOLID ELEMENTS
!
   area = 0.5*sqrt(((G(J+2)-G(I+2))*(G(K+3)-G(I+3))-(G(J+3)-G(I+3))*(G(K+2)-G(I+2)))                                                &
        & **2+((G(J+3)-G(I+3))*(G(K+1)-G(I+1))-(G(J+1)-G(I+1))*(G(K+3)-G(I+3)))**2+((G(J+1)-G(I+1))*(G(K+2)-G(I+2))-(G(J+2)-G(I+2)) &
        & *(G(K+1)-G(I+1)))**2)
END FUNCTION area