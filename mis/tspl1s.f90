
SUBROUTINE tspl1s(Ts1,Ts2,Ts6,Ts6s,Ts7,Ktr3,Ktr31)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A1 , A2 , A3 , Dista , Distb , Distc , Dum6(9) , Em(6) , Rj11 , Rj12 , Rj22 , X , Y , Z
   COMMON /matout/ Em , Dum6 , Rj11 , Rj12 , Rj22
   COMMON /sma1io/ X , Y , Z , Dista , Distb , Distc , A1 , A2 , A3
!
! Dummy argument declarations
!
   REAL Ktr3(400) , Ktr31(400) , Ts1(60) , Ts2(60) , Ts6(40) , Ts6s(40) , Ts7(60)
!
! Local variable declarations
!
   REAL a11 , a12 , a13 , a14 , a15 , a16 , a21 , a22 , a23 , a24 , a25 , a26 , a31 , a32 , a33 , a34 , a35 , a36 , a37 , be(7) ,   &
      & cons(2) , cons1 , cons11 , cons14 , d11 , d12 , d13 , d21 , d22 , d23 , d31 , d32 , d33 , ga(7) , ge1(9) , gs1(4) , j11 ,   &
      & j12 , j22 , thk , thk1 , wt(7)
   INTEGER i , ij , j , ji , k , kase
!
! End of declarations
!
!
!    TRANSVERSE SHEAR ROUTINE1 FOR CTRPLT1 - SINGLE PRECISION VERSION
!
!
   DATA be/0.33333333333333E0 , 0.47014206E0 , 0.05971588E0 , 0.47014206E0 , 0.101286505E0 , 0.79742699E0 , 0.101286505E0/ ,        &
      & ga/0.33333333333333E0 , 2*0.47014206E0 , 0.05971588E0 , 2*0.101286505E0 , 0.79742699E0/ , wt/0.1125E0 , 3*0.066197075E0 ,   &
      & 3*0.06296959E0/
   cons(1) = Dista*Distc
   cons(2) = Distb*Distc
   DO i = 1 , 60
      Ts1(i) = 0.0E0
   ENDDO
   DO k = 1 , 7
      DO kase = 1 , 2
         IF ( kase==1 ) X = be(k)*Dista
         IF ( kase==2 ) X = -be(k)*Distb
         Y = ga(k)*Distc
         CALL tspl3s(Ts6)
         cons1 = wt(k)*cons(kase)
         thk = A1 + A2*X + A3*Y
         cons14 = cons1*thk
         gs1(1) = Rj11*cons14
         gs1(2) = Rj12*cons14
         gs1(3) = gs1(2)
         gs1(4) = Rj22*cons14
         thk1 = thk**3/12.0E0
         cons11 = cons1*thk1
         d11 = Em(1)*thk1
         d12 = Em(2)*thk1
         d13 = Em(3)*thk1
         d22 = Em(4)*thk1
         d23 = Em(5)*thk1
         d33 = Em(6)*thk1
         d21 = d12
         d31 = d13
         d32 = d23
         j11 = 1.0/(Em(6)*thk)
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
         ge1(1) = Em(1)*cons11
         ge1(2) = Em(2)*cons11
         ge1(3) = Em(3)*cons11
         ge1(4) = ge1(2)
         ge1(5) = Em(4)*cons11
         ge1(6) = Em(5)*cons11
         ge1(7) = ge1(3)
         ge1(8) = ge1(6)
         ge1(9) = Em(6)*cons11
!
!        (B1) REFERS TO BENDING STRAIN DUE TO SECOND DERIVATIVES OF W
!        (B2) REFERS TO BENDING STRAINS DUE TO TRANSVERSE SHEAR STRAIN
!        (GAMMA) TRANSPOSE (GS) * (GAMMA) IS CONTRIBUTION OF STIFFNESS
!        MATRIX DUE TO WORK DONE BY SHEARING FORCES UNDERGOING SHEAR DEF
!
!
!  GAMMA TRANSPOSE GS GAMMA
!
         CALL gmmats(Ts6,2,20,+1,gs1,2,2,0,Ts6s)
         CALL gmmats(Ts6s,20,2,-2,Ts6,2,20,0,Ktr3)
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
         Ts1(46) = -120.0*a11*X
         Ts1(48) = -120.0*a21*X
         Ts1(49) = -12.0*(a32*X+a31*Y)
         Ts1(50) = -12.0*(a33*X+a21*Y)
         Ts1(51) = -12.0*(a36*X+a35*Y)
         Ts1(52) = -12.0*(a15*X+a32*Y)
         Ts1(53) = -12.0*(a34*X+a33*Y)
         Ts1(54) = -12.0*(a37*X+a36*Y)
         Ts1(55) = -24.0*a15*Y
         Ts1(56) = -24.0*(a25*X+a34*Y)
         Ts1(57) = -24.0*(a15*X+a37*Y)
         Ts1(59) = -120.0*a25*Y
         Ts1(60) = -120.0*a15*Y
!
!  B2 TRANSPOSE D B2
!
         CALL gmmats(Ts1,20,3,0,ge1,3,3,0,Ts2)
         CALL gmmats(Ts2,20,3,-2,Ts1,20,3,+1,Ktr3)
!
!  B2 TRANSPOSE D B1
!
         CALL tspl2s(Ts7)
         CALL gmmats(Ts2,20,3,0,Ts7,3,20,0,Ktr31)
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
END SUBROUTINE tspl1s
