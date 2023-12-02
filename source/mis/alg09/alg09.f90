!*==alg09.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg09
   USE c_ud300c
   USE c_ud3prt
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(21) :: ang , beta1 , dif , highm , pm1 , sol , talph1 , wd , wpara , ws , wt , xinc , xmr , xx1 , xx2 , xx3 ,   &
                         & xx4 , xx5 , xx6
   INTEGER :: ii , j , k , l1 , l2 , l3 , l4
   REAL :: q , wmax , x1 , x10 , x2 , x3 , x4 , x5 , x6 , x7 , x8 , x9 , xn
   EXTERNAL alg01 , alg03 , alg8 , alg9
!
! End of declarations rewritten by SPAG
!
!
!
!
!
   wmax = 0.7
   l1 = i + nl1(i)
   xn = speed(i)*spdfac(icase)*pi/(30.0*sclfac)
   IF ( iprint/=0 ) THEN
      l2 = abs(float(neval(i)))
      CALL alg03(lnct,7+nstrms)
      lnct = lnct - 3
      IF ( neval(i)>0 .AND. iprtc==1 ) WRITE (log2,99001) l1 , i , l2
99001 FORMAT (2X,/,8X,57HLOSS COEFFICIENT DETERMINATION FOR BLADE BETWEEN STATIONS,I3,4H AND,I3,                                    &
             &47H - AS INCORPORATED IN ABOVE RESULTS  BLADE TYPE,I2,/,8X,116(1H*),/,2X)
      IF ( neval(i)<0 .AND. iprtc==1 ) WRITE (log2,99002) l1 , i , l2
99002 FORMAT (2X,/,8X,57HLOSS COEFFICIENT DETERMINATION FOR BLADE BETWEEN STATIONS,I3,4H AND,I3,                                    &
             &47H - FOR PURPOSES OF COMPARISON ONLY   BLADE TYPE,I2,/,8X,116(1H*),/,2X)
   ENDIF
   l2 = ndimen(i) + 1
   IF ( l2==2 ) THEN
      DO j = 1 , nstrms
         xx2(j) = r(j,l1)/r(nstrms,l1)
         xx6(j) = r(j,i)/r(nstrms,i)
      ENDDO
   ELSEIF ( l2==3 ) THEN
      DO j = 1 , nstrms
         xx2(j) = xl(j,l1)
         xx6(j) = xl(j,i)
      ENDDO
   ELSEIF ( l2==4 ) THEN
      DO j = 1 , nstrms
         xx2(j) = xl(j,l1)/xl(nstrms,l1)
         xx6(j) = xl(j,i)/xl(nstrms,i)
      ENDDO
   ELSE
      DO j = 1 , nstrms
         xx2(j) = r(j,l1)
         xx6(j) = r(j,i)
      ENDDO
   ENDIF
   l2 = is2(i)
   CALL alg01(datac(l2),data5(l2),ndata(i),xx6,sol,x1,nstrms,nterp(i),0)
   q = 1.0
   IF ( speed(i)>=0.0 ) THEN
      IF ( speed(i)>0.0 ) THEN
         q = -1.0
      ELSEIF ( i>=3 ) THEN
         ii = i - 1
         DO WHILE ( speed(ii)==0.0 )
            IF ( ii==2 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            ii = ii - 1
         ENDDO
         IF ( speed(ii)<0.0 ) q = -1.0
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      DO J = 1 , Nstrms
         Talph1(J) = (vw(J,L1)-Xn*r(J,L1))/vm(J,L1)
         Dif(J) = 1.0 - vm(J,I)/vm(J,L1)*sqrt((1.0+tbeta(J,I)**2)/(1.0+Talph1(J)**2)) + (vm(J,L1)*Talph1(J)-vm(J,I)*tbeta(J,I))     &
                & /(2.0*Sol(J)*vm(J,L1)*sqrt(1.0+Talph1(J)**2))*Q
      ENDDO
      L2 = abs(float(neval(I)))
      L3 = ndiff(L2)
      CALL alg01(diff(1,L2),fdhub(1,L2),L3,Dif,Xx3,X1,Nstrms,0,0)
      CALL alg01(diff(1,L2),fdmid(1,L2),L3,Dif,Xx4,X1,Nstrms,0,0)
      CALL alg01(diff(1,L2),fdtip(1,L2),L3,Dif,Xx5,X1,Nstrms,0,0)
      Xx1(1) = 0.1
      Xx1(2) = 0.5
      Xx1(3) = 0.9
      DO J = 1 , Nstrms
         Xx1(4) = Xx3(J)
         Xx1(5) = Xx4(J)
         Xx1(6) = Xx5(J)
         X1 = (r(J,I)-r(1,I))/(r(Nstrms,I)-r(1,I))
         CALL alg01(Xx1,Xx1(4),3,X1,Wpara(J),X1,1,0,0)
      ENDDO
      DO J = 1 , Nstrms
         Xmr(J) = 0.0
         Highm(J) = 0.0
         Ang(J) = 0.0
         Ws(J) = 0.0
         Xinc(J) = 0.0
         Beta1(J) = 0.0
         Wd(J) = Wpara(J)*2.0*Sol(J)*sqrt(1.0+tbeta(J,I)**2)
         Wt(J) = Wd(J)
      ENDDO
      IF ( ndel(I)/=0 ) THEN
         L2 = is3(I)
         CALL alg01(delc(L2),delta(L2),ndel(I),Xx2,Pm1,X1,Nstrms,1,0)
         IF ( ndata(L1)/=0 ) THEN
            CALL alg01(r(1,L1),x(1,L1),Nstrms,r(1,L1),X1,Xx1,Nstrms,0,1)
            L2 = ndimen(L1) + 1
            IF ( L2==2 ) THEN
               DO J = 1 , Nstrms
                  Xx2(J) = r(J,L1)/r(J,Nstrms)
               ENDDO
            ELSEIF ( L2==3 ) THEN
               DO J = 1 , Nstrms
                  Xx2(J) = xl(J,L1)
               ENDDO
            ELSEIF ( L2==4 ) THEN
               DO J = 1 , Nstrms
                  Xx2(J) = xl(J,L1)/xl(Nstrms,L1)
               ENDDO
            ELSE
               DO J = 1 , Nstrms
                  Xx2(J) = r(J,L1)
               ENDDO
            ENDIF
            L2 = is2(L1)
            L3 = ndata(L1)
            CALL alg01(datac(L2),data1(L2),L3,Xx2,Xx3,X1,Nstrms,nterp(L1),0)
            CALL alg01(datac(L2),data3(L2),L3,Xx2,Xx4,X1,Nstrms,nterp(L1),0)
            DO J = 1 , Nstrms
               X1 = (atan((r(J,L1+1)-r(J,L1))/(x(J,L1+1)-x(J,L1)))+atan((r(J,L1)-r(J,L1-1))/(x(J,L1)-x(J,L1-1))))/2.0
               Beta1(J) = atan((tan(Xx3(J)/c1)*(1.0-Xx1(J)*tan(X1))-tan(X1)*tan(Xx4(J)/c1)*sqrt(1.0+Xx1(J)**2))*cos(X1))
               Xinc(J) = (atan(Talph1(J))-Beta1(J))*Q
            ENDDO
         ENDIF
         DO J = 1 , Nstrms
            Ang(J) = Xinc(J) + Pm1(J)/c1
            X1 = h(J,L1) - (vm(J,L1)**2+vw(J,L1)**2)/(2.0*g*ej)
            IF ( X1<hmin ) X1 = hmin
            X4 = alg8(X1,s(J,L1))
            X2 = (X4+1.0)/(X4-1.0)
            X3 = sqrt(X2)
            X5 = alg9(X1,s(J,L1),vm(J,L1)**2*(1.0+Talph1(J)**2))
            Xmr(J) = sqrt(X5)
            X6 = X5
            IF ( X6<1.0 ) X6 = 1.0
            X7 = X3*atan(sqrt(X6-1.0)/X3) - atan(sqrt(X6-1.0)) + Ang(J)
            X10 = 0.0
            IF ( X7>0.0 ) THEN
               X8 = 0.4*Pi*(X3-1.0)
               IF ( X7>X8 ) THEN
                  X10 = sqrt(X6-1.0)
               ELSE
                  X9 = 1.0
                  K = 1
                  SPAG_Loop_2_1: DO
                     X10 = X9 - (X2+X9*X9)*(1.0+X9*X9)/(X9*X9*(X2-1.0))*(X3*atan(X9/X3)-atan(X9)-X7)
                     IF ( abs(X10-X9)<=0.00001 ) EXIT SPAG_Loop_2_1
                     IF ( K>20 ) THEN
                        IF ( Iprint/=0 ) THEN
                           CALL alg03(Lnct,1)
                           WRITE (Log2,99001) ipass , I , J
99001                      FORMAT (5X,4HPASS,I3,9H  STATION,I3,12H  STREAMLINE,I3,                                                  &
                                  &58H  PRANDTL-MEYER FUNCTION NOT CONVERGED - USE INLET MACH NO)
                        ENDIF
                        X10 = sqrt(X6-1.0)
                        EXIT SPAG_Loop_2_1
                     ELSE
                        K = K + 1
                        X9 = X10
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDIF
            Highm(J) = sqrt(1.0+X10*X10)
            X1 = (Highm(J)+sqrt(X6))/2.0
            IF ( X5<1.0 ) X1 = X1*sqrt(X5)
            IF ( X1>1.0 ) THEN
               X1 = X1*X1
               Ws(J) = (((X4+1.0)*X1/((X4-1.0)*X1+2.0))**(X4/(X4-1.0))*((X4+1.0)/(2.0*X4*X1-X4+1.0))**(1.0/(X4-1.0))-1.0)           &
                     & /((1.0+(X4-1.0)/2.0*X5)**(X4/(1.0-X4))-1.0)
            ENDIF
            Wt(J) = Wd(J) + Ws(J)
         ENDDO
      ENDIF
      IF ( Iprint==1 ) THEN
         IF ( Lnct+3>npage ) THEN
            IF ( Iprtc/=0 ) WRITE (Log2,99002)
99002       FORMAT (1H1)
            Lnct = 4 + Nstrms
         ENDIF
         IF ( Iprtc==1 ) WRITE (Log2,99003)
99003    FORMAT (5X,                                                                                                                &
         &'STREAM  INLET   OUTLET  CASCADE   DIFF       LOSS   DIFFUSION  BLADE  INCIDENCE  EXPANSION INLET  EXPANDED SHOCK   TOTAL'&
        & ,/,5X,                                                                                                                    &
         &'-LINE   RADIUS  RADIUS  SOLIDITY  FACTOR  PARAMETER   LOSS     ANGLE    ANGLE      ANGLE    M.NO  MACH NO   LOSS   LOSS '&
        & ,/,2X)
         Lnct = Lnct + 3
         DO J = 1 , Nstrms
            X1 = Beta1(J)*c1*Q
            X2 = Xinc(J)*c1
            X3 = Ang(J)*c1
            IF ( Iprtc==1 ) WRITE (Log2,99004) J , r(J,L1) , r(J,I) , Sol(J) , Dif(J) , Wpara(J) , Wd(J) , X1 , X2 , X3 , Xmr(J) ,  &
                                 & Highm(J) , Ws(J) , Wt(J)
99004       FORMAT (I9,F10.3,F8.3,2F9.4,F10.5,F9.5,2F9.3,F10.3,F10.4,F8.4,F8.5,F9.5)
         ENDDO
      ELSE
         L2 = is2(I)
         L3 = nterp(I)
         L4 = ndata(I)
         IF ( nwork(I)>=5 ) CALL alg01(datac(L2),data6(L2),L4,Xx6,Xx5,X1,Nstrms,L3,0)
         CALL alg01(datac(L2),data1(L2),L4,Xx6,Xx1,X1,Nstrms,L3,0)
         CALL alg01(datac(L2),data4(L2),L4,Xx6,Xx4,X1,Nstrms,L3,0)
         CALL alg01(datac(L2),data3(L2),L4,Xx6,Xx3,X1,Nstrms,L3,0)
         ndata(I) = Nstrms
         L2 = L2 - 1
         DO J = 1 , Nstrms
            K = L2 + J
            datac(K) = Xx6(J)
            IF ( nwork(I)>=5 ) data6(K) = Xx5(J)
            data1(K) = Xx1(J)
            IF ( Wt(J)>Wmax ) Wt(J) = Wmax
            data2(K) = Wt(J)
            data3(K) = Xx3(J)
            data4(K) = Xx4(J)
            data5(K) = Sol(J)
         ENDDO
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE alg09
