!*==perpec.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE perpec(X,Stereo)
   USE c_blank
   USE c_rstxxx
   USE c_xxparm
   USE iso_fortran_env
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
   IF ( prject/=1 ) THEN
      IF ( fvp/=0 ) THEN
         IF ( prject==3 ) THEN
!
!     STEREO PROJECTION...FIND VANTAGE POINT
!
            r0 = rdist + aver(1)*objmod
            s0l = aver(2)*objmod - s0s/2.
            s0r = aver(2)*objmod + s0s/2.
            t0 = aver(3)*objmod
            d0 = d03
            RETURN
         ELSE
!
!     PERSPECTIVE PROJECTION...FIND VANTAGE POINT
!
            r = d(1)**2 + d(2)**2 + d(3)**2
            diam = dsqrt(r)
            r0 = 2.*diam + aver(1)
            s0l = aver(2)
            t0 = diam + aver(3)
            d0 = 1.5*diam
         ENDIF
      ENDIF
!
      scal = 1.
      IF ( prject==3 ) scal = objmod
      slr = s0l
      IF ( Stereo/=0 ) slr = s0r
      DO gp = 1 , ngpset
         r = d0/(r0-scal*X(1,gp))
         s = slr + r*(scal*X(2,gp)-slr)
         t = t0 + r*(scal*X(3,gp)-t0)
         X(2,gp) = s
         X(3,gp) = t
         IF ( prject/=3 ) THEN
            min(2) = amin1(min(2),s)
            min(3) = amin1(min(3),t)
            max(2) = amax1(max(2),s)
            max(3) = amax1(max(3),t)
         ENDIF
      ENDDO
      IF ( prject/=3 ) THEN
!
!     FIND MINIMA + MAXIMA DIFFERENCES + AVERAGES
!
         DO i = 2 , 3
            d(i) = max(i) - min(i)
            aver(i) = (max(i)+min(i))/2.
         ENDDO
      ENDIF
   ENDIF
!
END SUBROUTINE perpec
