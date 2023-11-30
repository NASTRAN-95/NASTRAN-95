
SUBROUTINE tlodt2(Ts1,Ts2)
   IMPLICIT NONE
   REAL A1 , A2 , A3 , B1 , B2 , B3 , D(3) , Dista , Distb , Distc , Dum6(9) , Em(6) , Est(100) , G1(3) , Rj11 , Rj12 , Rj22 , X ,  &
      & Y , Z
   COMMON /emgest/ Est
   COMMON /matout/ Em , Dum6 , Rj11 , Rj12 , Rj22
   COMMON /ssgwrk/ X , Y , Z , Dista , Distb , Distc , A1 , A2 , A3 , B1 , B2 , B3 , G1 , D
   REAL Ts1(60) , Ts2(20)
   REAL a11 , a12 , a13 , a14 , a15 , a16 , a21 , a22 , a23 , a24 , a25 , a26 , a31 , a32 , a33 , a34 , a35 , a36 , a37 , be(7) ,   &
      & cons(2) , cons1 , d11 , d12 , d13 , d21 , d22 , d23 , d31 , d32 , d33 , ga(7) , j11 , j12 , j22 , temp , thk , thk1 ,       &
      & ts3(20) , wt(7)
   INTEGER i , k , kase
!
!    CALCULATION OF PTGEN2 - GEN THERMAL LOAD VECTOR DUE TO TRANSVERSE S
!
   DATA be/0.33333333333333 , 0.47014206 , 0.05971588 , 0.47014206 , 0.101286505 , 0.79742699 , 0.101286505/
   DATA ga/0.33333333333333 , 2*.47014206 , 0.05971588 , 2*0.101286505 , 0.79742699/
   DATA wt/0.1125 , 3*0.066197075 , 3*0.06296959/
!
   cons(1) = Dista*Distc
   cons(2) = Distb*Distc
   DO i = 1 , 60
      Ts1(i) = 0.0
   ENDDO
   DO i = 1 , 20
      ts3(i) = 0.0
   ENDDO
   DO k = 1 , 7
      DO kase = 1 , 2
         IF ( kase==1 ) X = be(k)*Dista
         IF ( kase==2 ) X = -be(k)*Distb
         Y = ga(k)*Distc
         cons1 = wt(k)*cons(kase)
         thk = A1 + A2*X + A3*Y
         temp = D(1) + D(2)*X + D(3)*Y
         thk1 = thk**3/12.0
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
!
         CALL gmmats(Ts1,20,3,0,G1,3,1,0,Ts2)
         DO i = 1 , 20
            Ts2(i) = Ts2(i)*temp*thk1*cons1
            ts3(i) = ts3(i) + Ts2(i)
         ENDDO
      ENDDO
   ENDDO
   DO i = 1 , 20
      Ts2(i) = ts3(i)
   ENDDO
END SUBROUTINE tlodt2