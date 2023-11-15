
SUBROUTINE perpec(X,Stereo)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Aver(3) , Cstm(3,3) , D(3) , D0 , D02 , D03 , Max(3) , Min(3) , Objmod , Penpap(30) , R0 , S0l , S0r , S0s , Scalx1 ,       &
      & Scalx2(3) , Skp(5) , Skpplt(6) , T0 , View(15)
   INTEGER Fvp , Ngpset , Prject
   COMMON /blank / Skp , Ngpset
   COMMON /rstxxx/ Cstm , Min , Max , D , Aver
   COMMON /xxparm/ Skpplt , Penpap , Scalx1 , Objmod , Scalx2 , View , Fvp , R0 , S0l , S0r , T0 , D0 , D02 , D03 , Prject , S0s
!
! Dummy argument declarations
!
   INTEGER Stereo
   REAL X(3,1)
!
! Local variable declarations
!
   DOUBLE PRECISION diam , r
   INTEGER gp , i
   REAL rdist , s , scal , slr , t
!
! End of declarations
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
            GOTO 99999
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
99999 END SUBROUTINE perpec
