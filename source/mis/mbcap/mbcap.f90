!*==mbcap.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mbcap(Nphi,Capphi)
   IMPLICIT NONE
   USE C_MBOXC
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nphi
   COMPLEX , DIMENSION(1) :: Capphi
!
! Local variable declarations rewritten by SPAG
!
   REAL :: arg , arg1 , kbar , km , x , xb , xl , xr , xu
   INTEGER :: i , j , l
   REAL , DIMENSION(10) , SAVE :: p , w
   EXTERNAL go , zj
!
! End of declarations rewritten by SPAG
!
!
   !>>>>EQUIVALENCE (Km,Ekm) , (Kbar,Ekbar)
   DATA w/0.0506143 , 0.1111905 , 0.1568533 , 0.1813419 , 0.1813419 , 0.1568533 , 0.1111905 , 0.0506143 , 0.0 , 0.0/ , p/0.0198551 ,&
      & 0.1016667 , 0.2372338 , 0.4082826 , 0.5917174 , 0.7627662 , 0.8983333 , 0.9801449 , 0.0 , 0.0/
!
   DO i = 1 , Nphi
      Capphi(i) = (0.0,0.0)
   ENDDO
!
!     COMPUTE CAPPHI FOR RECEIVING BOX
!
   IF ( kbar<=0.0 ) THEN
!
      Capphi(1) = (-0.5,0.0)
   ELSE
      DO i = 1 , 8
         j = 9 - i
         arg = kbar*p(j)/2.0
         arg1 = w(i)*zj(arg/Mach)/2.0
         Capphi(1) = Capphi(1) + cmplx(-cos(arg)*arg1,sin(arg)*arg1)
      ENDDO
   ENDIF
!
!     COMPUTE REMAINING CAPPHI
!
   Nphi = 1
   xb = 0.5
   xu = xb + 1.0
   DO i = 2 , Ncb
      xl = -0.5
      xr = xl + 1.0
      DO j = 1 , i
         Nphi = Nphi + 1
         DO l = 1 , 8
            x = xb + p(l)
            arg = kbar*x
            arg1 = w(l)*go(x,xr,xl,km)/3.14159265
            Capphi(Nphi) = Capphi(Nphi) - cmplx(cos(arg)*arg1,-sin(arg)*arg1)
         ENDDO
         xl = xr
         xr = xr + 1.0
      ENDDO
!
      xb = xu
      xu = xb + 1.0
   ENDDO
!
   DO i = 1 , Nphi
      Capphi(i) = Boxw*Capphi(i)
   ENDDO
END SUBROUTINE mbcap
