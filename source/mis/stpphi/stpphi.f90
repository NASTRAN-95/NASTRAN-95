!*==stpphi.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE stpphi(Ca,Bloc,Pm,Ns)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ns
   REAL , DIMENSION(1) :: Ca
   REAL , DIMENSION(1) :: Bloc
   REAL , DIMENSION(37,Ns) :: Pm
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c , c2 , ct , pi , pmx , s , s2 , x
   INTEGER :: i , n
   REAL , DIMENSION(37) :: p
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!     PHI-FUNCTIONS FOR EACH STRIP (NACA TM 991, PG 19).
!     THE FOLLOWING FUNCTIONS ARE NOT COMPUTED, THEY ARE LEFT ZEROED
!         - NUMBERS  20, 22-30, 33, 34
   pi = 3.141593
   DO n = 1 , Ns
      DO i = 1 , 37
         p(i) = 0.0
      ENDDO
      ct = Ca(n)/Bloc(n)
      IF ( ct>1.0E-03 ) THEN
         c = ct - 1.0
         c2 = c*c
         s2 = 1.0 - c2
         s = sqrt(s2)
         x = atan2(s,c)
!     WATCH THIS TRIG
         pmx = pi - x
         p(1) = pmx + s
         p(2) = pmx*(1.+2.*c) + s*(2.+c)
         p(3) = pmx + s*c
         p(4) = pmx*2.*c + s*2.*(2.+c2)/3.
         p(5) = s*(1.-c)
         p(6) = 2.*pmx + s*2.*(2.-c)*(1.+2.*c)/3.
         p(7) = pmx*(0.5+2.*c) + s*(8.+5.*c+4.*c2-2.*c2*c)/6.
         p(8) = pmx*(-1.+2.*c) + s*(2.-c)
         p(9) = pmx*(1.+2.*c) + s*(2.+3.*c+4.*c2)/3.
         p(11) = p(2)*p(3)
         p(12) = pmx*pmx*(0.5+4.*c2) + pmx*s*c*(7.+2.*c2) + s2*(2.+2.5*c2)
         p(13) = sin(0.5*x)/cos(0.5*x)
         p(14) = 2.*s
         p(15) = p(13) - p(14)
         p(16) = p(1)*p(14)
         p(17) = p(3)**2 + s2*s2
         p(18) = -p(13)*(pmx*(1.+2.*c)-s*c)
         p(19) = p(3)*s
         p(21) = -2.*(c+alog(s2))
         p(31) = pmx - s
         p(32) = pmx + s*(1.+2.*c)
         p(35) = 2.*s2
         p(36) = p(32)*p(3) + 2.*s2*s2
         p(37) = p(3)*(p(2)-p(3))
         p(10) = p(31)*p(5)
      ENDIF
      DO i = 1 , 37
         Pm(i,n) = p(i)
      ENDDO
   ENDDO
END SUBROUTINE stpphi
