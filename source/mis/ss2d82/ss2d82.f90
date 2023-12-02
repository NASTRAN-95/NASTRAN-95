!*==ss2d82.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ss2d82(Ieqex,Neqex,Tgrid)
   IMPLICIT NONE
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ieqex
   INTEGER :: Neqex
   REAL , DIMENSION(8) :: Tgrid
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(8) :: dn
   REAL :: eltemp , gstemp , rgtemp , tref
   REAL , DIMENSION(8) , SAVE :: eta , xi
   REAL , DIMENSION(32) , SAVE :: ex2d82
   REAL , DIMENSION(72) , SAVE :: ex2d83
   REAL , DIMENSION(9) :: g , temp
   INTEGER :: i , i3 , id , id1 , idn , iii , isub , isub1 , isub2 , j , jii , jjj , jjsub , jsub , k , kk , n , n3 , ns , nx , ny
   INTEGER , DIMENSION(3) :: istres
   INTEGER , DIMENSION(1) :: iz , nph1 , nsil
   REAL , DIMENSION(3) :: pt , st
   REAL , DIMENSION(27) :: sigs
   REAL , DIMENSION(24) :: sigt
   REAL , DIMENSION(43) :: stress
   REAL , DIMENSION(48) :: ta
   EXTERNAL gmmats , mesage
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     PHASE 2 OF STRESS DATA RECOVERY FOR 2-D, 8 GRID POINT
!     ISOPARAMETRIC STRUCTURAL ELEMENT
!
!     PH1OUT CONTAINS THE FOLLOWING
!     ELEMENT ID
!     8 SILS
!     TREF
!     ST ARRAY
!     TRANSFORMATION MATRIX FROM GLOBAL TO ELEMENT COORDINATES
!     COORD SYSTEM ID FOR STRESS OUTPUT
!     G MATRIX
!     DNX,DNY AT EACH GRID POINT -EVALUATED 8 TIMES
!
!
   !>>>>EQUIVALENCE (Ph1out(1),Nph1(1)) , (Ph1out(1),Id) , (Nsil(1),Ph1out(2)) , (Tref,Ph1out(10)) , (St(1),Ph1out(11)) ,                &
!>>>>    & (Ta(1),Ph1out(14)) , (G(1),Ph1out(63)) , (Ph1out(62),Id1) , (istres(1),stress(1)) , (Ldtemp,Eltemp) , (Z(1),Iz(1))
   DATA ex2d82/1.86603 , -.50000 , -.50000 , .13397 , -.50000 , .13397 , 1.86603 , -.50000 , .13397 , -.50000 , -.50000 , 1.86603 , &
      & -.50000 , 1.86603 , .13397 , -.50000 , .68301 , -.18301 , .68301 , -.18301 , -.18301 , -.18301 , .68301 , .68301 , -.18301 ,&
      & .68301 , -.18301 , .68301 , .68301 , .68301 , -.18301 , -.18301/
   DATA ex2d83/2.18694 , -.98589 , .27778 , -.98589 , .44444 , -.12522 , .27778 , -.12522 , .03528 , .27778 , -.12522 , .03528 ,    &
      & -.98589 , .44444 , -.12522 , 2.18694 , -.98589 , .27778 , .03528 , -.12522 , .27778 , -.12522 , .44444 , -.98589 , .27778 , &
      & -.98589 , 2.18694 , .27778 , -.98589 , 2.18694 , -.12522 , .44444 , -.98589 , .03528 , -.12522 , .27778 , -.00000 ,         &
      & 0.00000 , -.00000 , 1.47883 , -.66667 , .18784 , .00000 , -.00000 , -.00000 , -.00000 , .18784 , .00000 , -.00000 ,         &
      & -.66667 , -.00000 , .00000 , 1.47883 , .00000 , -.00000 , 0.00000 , .00000 , .18784 , -.66667 , 1.47883 , .00000 , -.00000 ,&
      & .00000 , -.00000 , 1.47883 , .00000 , -.00000 , -.66667 , -.00000 , -.00000 , .18784 , 0.00000/
   DATA xi/ - 1. , 1. , 1. , -1. , 0. , 1. , 0. , -1./
   DATA eta/ - 1. , -1. , 1. , 1. , -1. , 0. , 1. , 0./
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SET UP DISPLACEMENTS FOR THIS ELEMENT
!
         Is = 0
         DO i = 1 , 8
            Nstrt = Ivec + nsil(i) - 1
            DO j = 1 , 3
               Is = Is + 1
               Npt = Nstrt + j - 1
               Disp(Is) = Z(Npt)
            ENDDO
         ENDDO
!
!     INITIALIZE SOME MATRICES
!
         DO i = 1 , 72
            Bb(i) = 0.
         ENDDO
!
!     SET UP INDICATOR FOR GRID POINT TEMPERATURES
!
         Idtemp = 0
         DO i = 1 , 8
            IF ( Tgrid(i)/=0. ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         Idtemp = 1
         spag_nextblock_1 = 3
      CASE (3)
!
!     START LOOPING FOR STRESSES
!
         idn = 4
         IF ( id1==3 ) idn = 9
         iii = 0
         pt(1) = -0.57735027
         pt(2) = -pt(1)
         IF ( id1/=2 ) THEN
            pt(1) = -0.77459667
            pt(2) = 0.
            pt(3) = -pt(1)
         ENDIF
         DO jii = 1 , id1
            DO jjj = 1 , id1
               iii = iii + 1
!
!     COMPUTE BASE POINTER FOR PICKING UP DERIVATIVES
!
               Ibase = 71 + 16*(iii-1)
!
               DO n = 1 , 8
                  nx = n + Ibase
                  ny = n + Ibase + 8
                  Dnx(n) = Ph1out(nx)
                  Dny(n) = Ph1out(ny)
               ENDDO
!
               DO n = 1 , 8
!
!     SET UP THE B MATRIX
!
                  DO i = 1 , 9
                     temp(i) = 0.
                     B(i) = 0.
                  ENDDO
                  B(1) = Dnx(n)
                  B(4) = Dny(n)
                  B(5) = Dny(n)
                  B(6) = Dnx(n)
!
!     TRANSFORM TO ELEMENT COORDINATES
!
                  kk = 6*n - 6
                  DO i = 1 , 6
                     k = kk + i
                     Tb(i) = ta(k)
                  ENDDO
                  CALL gmmats(B,3,2,0,Tb,2,3,0,temp(1))
                  n3 = 3*n
                  Bb(n3-2) = temp(1)
                  Bb(n3-1) = temp(2)
                  Bb(n3) = temp(3)
                  Bb(n3+22) = temp(4)
                  Bb(n3+23) = temp(5)
                  Bb(n3+24) = temp(6)
                  Bb(n3+46) = temp(7)
                  Bb(n3+47) = temp(8)
                  Bb(n3+48) = temp(9)
               ENDDO
!
!     BRING IN G MATRIX
!
               CALL gmmats(g,3,3,0,Bb,3,24,0,Db)
!
!     COMPUTE STRESSES
!
               CALL gmmats(Db,3,24,0,Disp,24,1,0,Sig)
!
!     STORE GAUSS POINT STRESSES INTO SIGT
!
               i3 = 3*(iii-1)
               DO i = 1 , 3
                  isub = i3 + i
                  sigs(isub) = Sig(i)
               ENDDO
!
!     COMPUTE GAUSS POINT  TEMPERATURES
!
               IF ( Ldtemp/=-1 ) THEN
                  IF ( Idtemp==1 ) THEN
!
!     ALL TEMPERATURES ARE DEFAULT VALUE
!
                     DO n = 1 , 4
                        dn(n) = .25*(1.+pt(jii)*xi(n))*(1.+pt(jjj)*eta(n))*(pt(jii)*xi(n)+pt(jjj)*eta(n)-1.)
                     ENDDO
                     DO n = 5 , 7 , 2
                        dn(n) = .5*(1.-pt(jii)*pt(jii))*(1.+pt(jjj)*eta(n))
                     ENDDO
                     DO n = 6 , 8 , 2
                        dn(n) = .5*(1.+pt(jii)*xi(n))*(1.-pt(jjj)*pt(jjj))
                     ENDDO
                     gstemp = 0.
                     DO n = 1 , 8
                        gstemp = gstemp + dn(n)*Tgrid(n)
                     ENDDO
                     rgtemp = gstemp - tref
                  ELSE
                     rgtemp = eltemp - tref
                  ENDIF
                  DO i = 1 , 3
                     isub = i3 + i
                     sigs(isub) = sigs(isub) - st(i)*rgtemp
                  ENDDO
               ENDIF
!
            ENDDO
         ENDDO
!
!     MULTIPLY BY TRANSFORMATION FROM GAUSS POINTS TO GRID POINTS
!
         IF ( id1==2 ) CALL gmmats(ex2d82,8,4,0,sigs,4,3,0,sigt)
         IF ( id1==3 ) CALL gmmats(ex2d83,8,9,0,sigs,9,3,0,sigt)
!
!     FINISH UP
!
         DO iii = 1 , 8
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
!     MOVE A ROW OF SIGT INTO SIG
!
                  i3 = 3*(iii-1)
                  DO i = 1 , 3
                     isub = i3 + i
                     Sig(i) = sigt(isub)
                  ENDDO
!
!     STORE STRESSES
!
                  jsub = 5*(iii-1) + 4
                  isub1 = Ieqex + 1
                  isub2 = Ieqex + Neqex - 1
                  DO jjj = isub1 , isub2 , 2
                     ns = iz(jjj)/10
                     IF ( ns==nsil(iii) ) THEN
                        istres(jsub) = iz(jjj-1)
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
                  CALL mesage(-30,164,iz(jjj))
                  spag_nextblock_2 = 2
               CASE (2)
                  istres(jsub+1) = 0
                  DO i = 1 , 3
                     jjsub = jsub + 1 + i
                     stress(jjsub) = Sig(i)
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
!     LOOP FOR OTHER GRID POINTS
!
         ENDDO
!
!     FINISH UP
!
!     ELEMENT ID
!
         istres(1) = id
!
!     NUMBER OF GRID POINTS PER ELEMENT
!
         istres(2) = 8
!
!     NUMBER OF STRESSES OUTPUT PER ELEMENT
!
         istres(3) = 3
!
         DO i = 1 , 43
            Str(i) = stress(i)
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE ss2d82
