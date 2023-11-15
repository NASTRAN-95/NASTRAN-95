
SUBROUTINE ss2d82(Ieqex,Neqex,Tgrid)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL B(9) , Bb(72) , Db(72) , Deform , Disp(24) , Dnx(8) , Dny(8) , Dnz , Dummy(35) , Eltemp , Forvec(250) , G(9) , Ph1out(100) ,&
      & Sig(3) , St(3) , Str(250) , Ta(48) , Tb(6) , Tref , Z(1)
   INTEGER Ibase , Id , Id1 , Idtemp , Is , Ivec , Ivecn , Iz(1) , Ldtemp , Nph1(1) , Npt , Nsil(1) , Nstrt
   COMMON /sdr2x4/ Dummy , Ivec , Ivecn , Ldtemp , Deform
   COMMON /sdr2x7/ Ph1out , Str , Forvec
   COMMON /sdr2x8/ Disp , Dnx , Dny , Dnz , B , Tb , Bb , Db , Sig , Ibase , Nstrt , Npt , Is , Idtemp
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Ieqex , Neqex
   REAL Tgrid(8)
!
! Local variable declarations
!
   REAL dn(8) , eta(8) , ex2d82(32) , ex2d83(72) , gstemp , pt(3) , rgtemp , sigs(27) , sigt(24) , stress(43) , temp(9) , xi(8)
   INTEGER i , i3 , idn , iii , istres(3) , isub , isub1 , isub2 , j , jii , jjj , jjsub , jsub , k , kk , n , n3 , ns , nx , ny
!
! End of declarations
!
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
   EQUIVALENCE (Ph1out(1),Nph1(1)) , (Ph1out(1),Id) , (Nsil(1),Ph1out(2)) , (Tref,Ph1out(10)) , (St(1),Ph1out(11)) ,                &
    & (Ta(1),Ph1out(14)) , (G(1),Ph1out(63)) , (Ph1out(62),Id1) , (istres(1),stress(1)) , (Ldtemp,Eltemp) , (Z(1),Iz(1))
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
!
!     SET UP DISPLACEMENTS FOR THIS ELEMENT
!
   Is = 0
   DO i = 1 , 8
      Nstrt = Ivec + Nsil(i) - 1
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
      IF ( Tgrid(i)/=0. ) GOTO 100
   ENDDO
   GOTO 200
 100  Idtemp = 1
!
!     START LOOPING FOR STRESSES
!
 200  idn = 4
   IF ( Id1==3 ) idn = 9
   iii = 0
   pt(1) = -0.57735027
   pt(2) = -pt(1)
   IF ( Id1/=2 ) THEN
      pt(1) = -0.77459667
      pt(2) = 0.
      pt(3) = -pt(1)
   ENDIF
   DO jii = 1 , Id1
      DO jjj = 1 , Id1
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
               Tb(i) = Ta(k)
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
         CALL gmmats(G,3,3,0,Bb,3,24,0,Db)
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
               rgtemp = gstemp - Tref
            ELSE
               rgtemp = Eltemp - Tref
            ENDIF
            DO i = 1 , 3
               isub = i3 + i
               sigs(isub) = sigs(isub) - St(i)*rgtemp
            ENDDO
         ENDIF
!
      ENDDO
   ENDDO
!
!     MULTIPLY BY TRANSFORMATION FROM GAUSS POINTS TO GRID POINTS
!
   IF ( Id1==2 ) CALL gmmats(ex2d82,8,4,0,sigs,4,3,0,sigt)
   IF ( Id1==3 ) CALL gmmats(ex2d83,8,9,0,sigs,9,3,0,sigt)
!
!     FINISH UP
!
   DO iii = 1 , 8
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
         ns = Iz(jjj)/10
         IF ( ns==Nsil(iii) ) THEN
            istres(jsub) = Iz(jjj-1)
            GOTO 250
         ENDIF
      ENDDO
      CALL mesage(-30,164,Iz(jjj))
 250  istres(jsub+1) = 0
      DO i = 1 , 3
         jjsub = jsub + 1 + i
         stress(jjsub) = Sig(i)
      ENDDO
!
!     LOOP FOR OTHER GRID POINTS
!
   ENDDO
!
!     FINISH UP
!
!     ELEMENT ID
!
   istres(1) = Id
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
!
END SUBROUTINE ss2d82
