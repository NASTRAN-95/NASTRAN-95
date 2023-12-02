!*==tspl1d.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE tspl1d(Ts1,Ts2,Ts6,Ts6s,Ts7,Ktr3,Ktr31)
   USE c_matout
   USE c_sma1io
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(60) :: Ts1
   REAL(REAL64) , DIMENSION(60) :: Ts2
   REAL(REAL64) , DIMENSION(40) :: Ts6
   REAL(REAL64) , DIMENSION(40) :: Ts6s
   REAL(REAL64) , DIMENSION(60) :: Ts7
   REAL(REAL64) , DIMENSION(400) :: Ktr3
   REAL(REAL64) , DIMENSION(400) :: Ktr31
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: a11 , a12 , a13 , a14 , a15 , a16 , a21 , a22 , a23 , a24 , a25 , a26 , a31 , a32 , a33 , a34 , a35 , a36 , a37 ,&
                 & cons1 , cons11 , cons14 , thk
   REAL(REAL64) , DIMENSION(7) , SAVE :: be , ga , wt
   REAL(REAL64) , DIMENSION(2) :: cons
   REAL :: d11 , d12 , d13 , d21 , d22 , d23 , d31 , d32 , d33 , j11 , j12 , j22 , thk1
   REAL(REAL64) , DIMENSION(9) :: ge1
   REAL(REAL64) , DIMENSION(4) :: gs1
   INTEGER :: i , ij , j , ji , k , kase
   EXTERNAL gmmatd , tspl2d , tspl3d
!
! End of declarations rewritten by SPAG
!
!
!    TRANSVERSE SHEAR ROUTINE1 FOR CTRPLT1 - DOUBLE PRECISION VERSION
!
   DATA be/0.33333333333333333333D0 , 0.47014206D0 , 0.05971588D0 , 0.47014206D0 , 0.101286505D0 , 0.79742699D0 , 0.101286505D0/ ,  &
      & ga/0.33333333333333333333D0 , 2*0.47014206D0 , 0.05971588D0 , 2*0.101286505D0 , 0.79742699D0/ , wt/0.1125D0 ,               &
      & 3*0.066197075D0 , 3*0.06296959D0/
   cons(1) = dista*distc
   cons(2) = distb*distc
   DO i = 1 , 60
      Ts1(i) = 0.0D0
   ENDDO
   DO k = 1 , 7
      DO kase = 1 , 2
         IF ( kase==1 ) x = be(k)*dista
         IF ( kase==2 ) x = -be(k)*distb
         y = ga(k)*distc
         CALL tspl3d(Ts6)
         cons1 = wt(k)*cons(kase)
         thk = a1 + a2*x + a3*y
         cons14 = cons1*thk
         gs1(1) = rj11*cons14
         gs1(2) = rj12*cons14
         gs1(3) = gs1(2)
         gs1(4) = rj22*cons14
         cons11 = cons1*thk**3/12.0D0
         thk1 = thk**3/12.0D0
         d11 = em(1)*thk1
         d12 = em(2)*thk1
         d13 = em(3)*thk1
         d22 = em(4)*thk1
         d23 = em(5)*thk1
         d33 = em(6)*thk1
         d21 = d12
         d31 = d13
         d32 = d23
         j11 = 1.0/(em(6)*thk)
         j22 = j11
         j12 = 0.0
         a11 = -(j11*d11+j12*d13)
         a12 = -(j11*d12+j12*d23)
         a13 = -(j11*d13+j12*d33)
         a14 = -(j11*d31+j12*d21)
         a15 = -(j11*d32+j12*d22)
         a16 = -(j11*d33+j12*d23)
         a21 = -(j12*d11+j22*d13)
         a22 = -(j12*d12+j22*d23)
         a23 = -(j12*d13+j22*d33)
         a24 = -(j12*d13+j22*d12)
         a25 = -(j12*d23+j22*d22)
         a26 = -(j12*d33+j22*d32)
         a31 = a14 + 2.0*a13
         a32 = a12 + 2.0*a16
         a33 = a24 + 2.0*a23
         a34 = a22 + 2.0*a26
         a35 = a33 + a11
         a36 = a34 + a31
         a37 = a25 + a32
         ge1(1) = em(1)*cons11
         ge1(2) = em(2)*cons11
         ge1(3) = em(3)*cons11
         ge1(4) = ge1(2)
         ge1(5) = em(4)*cons11
         ge1(6) = em(5)*cons11
         ge1(7) = ge1(3)
         ge1(8) = ge1(6)
         ge1(9) = em(6)*cons11
!
!        (B1) REFERS TO BENDING STRAIN DUE TO SECOND DERIVATIVES OF W
!        (B2) REFERS TO BENDING STRAINS DUE TO TRANSVERSE SHEAR STRAIN
!        (GAMMA) TRANSPOSE (GS) * (GAMMA) IS CONTRIBUTION OF STIFFNESS
!        MATRIX DUE TO WORK DONE BY SHEARING FORCES UNDERGOING SHEAR DEF
!
!
!  GAMMA TRANSPOSE GS GAMMA
!
         CALL gmmatd(Ts6,2,20,+1,gs1,2,2,0,Ts6s)
         CALL gmmatd(Ts6s,20,2,-2,Ts6,2,20,0,Ktr3)
         Ts1(31) = -24.0*a11
         Ts1(33) = -24.0*a21
         Ts1(34) = -6.0*a31
         Ts1(35) = -6.0*a21
         Ts1(36) = -6.0*a35
         Ts1(37) = -4.0*a32
         Ts1(38) = -4.0*a33
         Ts1(39) = -4.0*a36
         Ts1(40) = -6.0*a15
         Ts1(41) = -6.0*a34
         Ts1(42) = -6.0*a37
         Ts1(44) = -24.0*a25
         Ts1(45) = -24.0*a15
         Ts1(46) = -120.0*a11*x
         Ts1(48) = -120.0*a21*x
         Ts1(49) = -12.0*(a32*x+a31*y)
         Ts1(50) = -12.0*(a33*x+a21*y)
         Ts1(51) = -12.0*(a36*x+a35*y)
         Ts1(52) = -12.0*(a15*x+a32*y)
         Ts1(53) = -12.0*(a34*x+a33*y)
         Ts1(54) = -12.0*(a37*x+a36*y)
         Ts1(55) = -24.0*a15*y
         Ts1(56) = -24.0*(a25*x+a34*y)
         Ts1(57) = -24.0*(a15*x+a37*y)
         Ts1(59) = -120.0*a25*y
         Ts1(60) = -120.0*a15*y
!
!  B2 TRANSPOSE D B2
!
         CALL gmmatd(Ts1,20,3,0,ge1,3,3,0,Ts2)
         CALL gmmatd(Ts2,20,3,-2,Ts1,20,3,+1,Ktr3)
!
!  B2 TRANSPOSE D B1
!
         CALL tspl2d(Ts7)
         CALL gmmatd(Ts2,20,3,0,Ts7,3,20,0,Ktr31)
!
!  B1 TRANSPOSE D B2
!
         DO i = 1 , 20
            DO j = 1 , 20
               ij = (i-1)*20 + j
               ji = (j-1)*20 + i
               Ktr3(ij) = Ktr3(ij) + Ktr31(ij) + Ktr31(ji)
            ENDDO
         ENDDO
      ENDDO
   ENDDO
END SUBROUTINE tspl1d
