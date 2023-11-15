
SUBROUTINE hdcoef(X,Y,Z,Xxx,Jxx,Ns,Ccc,Lz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER L0 , L00 , L01 , L1 , L10 , L11 , L12 , L13 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , Zcoef , Zcoef1
   REAL Rz(1) , X1skt , Xasolv , Xcc , Xdum , Y1skt , Yasolv , Z1skt , Zasolv
   COMMON /go3   / L0 , L1 , L00 , L01 , L2 , L3 , L4 , L5 , L6 , L7 , L8 , L9 , L10 , L11 , L12 , L13
   COMMON /hdptrs/ Xdum , Xcc , Xasolv , Yasolv , Zasolv , X1skt , Y1skt , Z1skt , Zcoef1 , Zcoef
   COMMON /zzzzzz/ Rz
!
! Dummy argument declarations
!
   INTEGER Jxx , Lz , Ns
   REAL Ccc(1) , X(1) , Xxx(1) , Y(1) , Z(1)
!
! Local variable declarations
!
   REAL a1 , a2 , b1 , b2 , c1 , c2 , coe(8) , e , epsi , f , p , s , t , t1 , u
   INTEGER i , ibcoef(5) , ix , j , ja , japj , jf , jfpk , k , k1 , k2 , k3 , le
!
! End of declarations
!
!
!
!     THIS SUBROUTINE DETERMINES EQUATION OF LINES AND PLANES.
!
!
   DATA epsi/1.0E-5/
   le = 0
   ja = L13 + (Jxx-1)*Lz
   jf = L12 + (Jxx-1)*5
   i = 0
   j = 1
   DO
!
!
!     SEARCH FOR MATCHING COORDINATES.
!
!
      i = i + 1
      t = X(i+1) - X(i)
      s = Y(i+1) - Y(i)
      u = Z(i+1) - Z(i)
      IF ( abs(t)<=epsi ) THEN
         IF ( abs(s)<=epsi ) THEN
!
!
!     MATCH FOUND.....PROCEED IF LIST IS NOT EXHAUSTED.
!
!
            IF ( abs(u)<=epsi ) i = i + 2
         ENDIF
      ENDIF
      IF ( i>Ns ) THEN
!
!
!     DETERMINE EQUATION OF PLANE.
!
         j = (j-1)/5
         Xxx(jf+5) = j
         IF ( Ns<=3 ) THEN
            Xxx(jf+5) = 1
            DO ix = 1 , 2
               Xxx(jf+ix) = Z(ix)
            ENDDO
            Xxx(jf+3) = 0
         ELSE
            k1 = 1
            k2 = 2
            k3 = 3
            a1 = X(k3) - X(k1)
            b1 = Y(k3) - Y(k1)
            c1 = Z(k3) - Z(k1)
            a2 = X(k2) - X(k1)
            b2 = Y(k2) - Y(k1)
            c2 = Z(k2) - Z(k1)
            coe(1) = b1*c2 - b2*c1
            coe(2) = c1*a2 - c2*a1
            coe(3) = a1*b2 - a2*b1
            coe(4) = coe(1)*X(1) + coe(2)*Y(1) + coe(3)*Z(1)
            coe(4) = -coe(4)
            DO j = 1 , 4
               Xxx(jf+j) = coe(j)
            ENDDO
            IF ( abs(coe(3))<=epsi ) THEN
               j = 1
               DO k = 1 , le
                  japj = ja + j
                  Ccc(japj) = Rz(Zcoef1-1+k)
                  Ccc(japj+1) = Rz(Zcoef-1+k)
                  j = j + 5
               ENDDO
               IF ( abs(coe(1))>epsi ) i = 1
               IF ( abs(coe(2))>epsi ) i = 2
               p = coe(i)
               IF ( abs(p)<epsi ) p = epsi
               DO k = 1 , 4
                  jfpk = jf + k
                  Xxx(jfpk) = Xxx(jfpk)/p
               ENDDO
            ENDIF
         ENDIF
         EXIT
      ELSE
!
!
!     DETERMINE EQUATION OF LINE-SEGMENTS.
!
!
         t = X(i+1) - X(i)
         t1 = Y(i+1) - Y(i)
         IF ( (abs(t1)<epsi) .AND. (abs(t)<epsi) ) CYCLE
         IF ( abs(t)>epsi ) THEN
            Ccc(j+ja) = 1
            e = (Y(i+1)-Y(i))/(X(i+1)-X(i))
            IF ( abs(e)<=100000. ) THEN
               f = (e*X(i)) - Y(i)
               Ccc(j+1+ja) = -e
               Ccc(j+2+ja) = f
               GOTO 20
            ENDIF
         ENDIF
         Ccc(j+ja) = 0
         Ccc(j+1+ja) = 1
         Ccc(j+2+ja) = -X(i)
 20      IF ( abs(Ccc(j+ja))>epsi ) THEN
            Ccc(j+3+ja) = X(i)
            Ccc(j+4+ja) = X(i+1)
         ELSE
            Ccc(j+3+ja) = Y(i)
            Ccc(j+4+ja) = Y(i+1)
         ENDIF
         j = j + 5
         Rz(Zcoef1+le) = Z(i)
         Rz(Zcoef+le) = Z(i+1)
         le = le + 1
         IF ( le<=3 ) ibcoef(le) = i
      ENDIF
   ENDDO
END SUBROUTINE hdcoef
