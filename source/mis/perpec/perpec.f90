!*==perpec.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE perpec(X,Stereo)
USE C_BLANK
USE C_RSTXXX
USE C_XXPARM
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(3,1) :: X
   INTEGER :: Stereo
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: diam , r
   INTEGER :: gp , i
   REAL , SAVE :: rdist
   REAL :: s , scal , slr , t
!
! End of declarations rewritten by SPAG
!
!
   DATA rdist/29./
!
!                               I====================I
!                         T     I                    I
!                         1     I     PROJECTION     I
!                         1     I                    I
!                         1     I       PLANE        I
!                         1     I                    I
!                         1     I====================I
!                         1    /                    /
!                         1   /                    /
!                         1  /   * OBSERVER       /
!                         1 /    1               /D0
!                         1/     1              /
!                         +--------------------/-----S
!                        /       1  /         /
!                       /      T01 /R0
!                      /         1/         /
!                     /----------+- - - - -/
!                    /    S0
!                   R
!
   IF ( Prject/=1 ) THEN
      IF ( Fvp/=0 ) THEN
         IF ( Prject==3 ) THEN
!
!     STEREO PROJECTION...FIND VANTAGE POINT
!
            R0 = rdist + Aver(1)*Objmod
            S0l = Aver(2)*Objmod - S0s/2.
            S0r = Aver(2)*Objmod + S0s/2.
            T0 = Aver(3)*Objmod
            D0 = D03
            RETURN
         ELSE
!
!     PERSPECTIVE PROJECTION...FIND VANTAGE POINT
!
            r = D(1)**2 + D(2)**2 + D(3)**2
            diam = dsqrt(r)
            R0 = 2.*diam + Aver(1)
            S0l = Aver(2)
            T0 = diam + Aver(3)
            D0 = 1.5*diam
         ENDIF
      ENDIF
!
      scal = 1.
      IF ( Prject==3 ) scal = Objmod
      slr = S0l
      IF ( Stereo/=0 ) slr = S0r
      DO gp = 1 , Ngpset
         r = D0/(R0-scal*X(1,gp))
         s = slr + r*(scal*X(2,gp)-slr)
         t = T0 + r*(scal*X(3,gp)-T0)
         X(2,gp) = s
         X(3,gp) = t
         IF ( Prject/=3 ) THEN
            Min(2) = amin1(Min(2),s)
            Min(3) = amin1(Min(3),t)
            Max(2) = amax1(Max(2),s)
            Max(3) = amax1(Max(3),t)
         ENDIF
      ENDDO
      IF ( Prject/=3 ) THEN
!
!     FIND MINIMA + MAXIMA DIFFERENCES + AVERAGES
!
         DO i = 2 , 3
            D(i) = Max(i) - Min(i)
            Aver(i) = (Max(i)+Min(i))/2.
         ENDDO
      ENDIF
   ENDIF
!
END SUBROUTINE perpec
