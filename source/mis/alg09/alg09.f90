!*==alg09.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE alg09
   IMPLICIT NONE
   USE C_UD300C
   USE C_UD3PRT
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
   l1 = I + Nl1(I)
   xn = Speed(I)*Spdfac(Icase)*Pi/(30.0*Sclfac)
   IF ( Iprint/=0 ) THEN
      l2 = abs(float(Neval(I)))
      CALL alg03(Lnct,7+Nstrms)
      Lnct = Lnct - 3
      IF ( Neval(I)>0 .AND. Iprtc==1 ) WRITE (Log2,99001) l1 , I , l2
99001 FORMAT (2X,/,8X,57HLOSS COEFFICIENT DETERMINATION FOR BLADE BETWEEN STATIONS,I3,4H AND,I3,                                    &
             &47H - AS INCORPORATED IN ABOVE RESULTS  BLADE TYPE,I2,/,8X,116(1H*),/,2X)
      IF ( Neval(I)<0 .AND. Iprtc==1 ) WRITE (Log2,99002) l1 , I , l2
99002 FORMAT (2X,/,8X,57HLOSS COEFFICIENT DETERMINATION FOR BLADE BETWEEN STATIONS,I3,4H AND,I3,                                    &
             &47H - FOR PURPOSES OF COMPARISON ONLY   BLADE TYPE,I2,/,8X,116(1H*),/,2X)
   ENDIF
   l2 = Ndimen(I) + 1
   IF ( l2==2 ) THEN
      DO j = 1 , Nstrms
         xx2(j) = R(j,l1)/R(Nstrms,l1)
         xx6(j) = R(j,I)/R(Nstrms,I)
      ENDDO
   ELSEIF ( l2==3 ) THEN
      DO j = 1 , Nstrms
         xx2(j) = Xl(j,l1)
         xx6(j) = Xl(j,I)
      ENDDO
   ELSEIF ( l2==4 ) THEN
      DO j = 1 , Nstrms
         xx2(j) = Xl(j,l1)/Xl(Nstrms,l1)
         xx6(j) = Xl(j,I)/Xl(Nstrms,I)
      ENDDO
   ELSE
      DO j = 1 , Nstrms
         xx2(j) = R(j,l1)
         xx6(j) = R(j,I)
      ENDDO
   ENDIF
   l2 = Is2(I)
   CALL alg01(Datac(l2),Data5(l2),Ndata(I),xx6,sol,x1,Nstrms,Nterp(I),0)
   q = 1.0
   IF ( Speed(I)>=0.0 ) THEN
      IF ( Speed(I)>0.0 ) THEN
         q = -1.0
      ELSEIF ( I>=3 ) THEN
         ii = I - 1
         DO WHILE ( Speed(ii)==0.0 )
            IF ( ii==2 ) THEN
               CALL spag_block_1
               RETURN
            ENDIF
            ii = ii - 1
         ENDDO
         IF ( Speed(ii)<0.0 ) q = -1.0
      ENDIF
   ENDIF
   CALL spag_block_1
CONTAINS
   SUBROUTINE spag_block_1
      DO j = 1 , Nstrms
         talph1(j) = (Vw(j,l1)-xn*R(j,l1))/Vm(j,l1)
         dif(j) = 1.0 - Vm(j,I)/Vm(j,l1)*sqrt((1.0+Tbeta(j,I)**2)/(1.0+talph1(j)**2)) + (Vm(j,l1)*talph1(j)-Vm(j,I)*Tbeta(j,I))     &
                & /(2.0*sol(j)*Vm(j,l1)*sqrt(1.0+talph1(j)**2))*q
      ENDDO
      l2 = abs(float(Neval(I)))
      l3 = Ndiff(l2)
      CALL alg01(Diff(1,l2),Fdhub(1,l2),l3,dif,xx3,x1,Nstrms,0,0)
      CALL alg01(Diff(1,l2),Fdmid(1,l2),l3,dif,xx4,x1,Nstrms,0,0)
      CALL alg01(Diff(1,l2),Fdtip(1,l2),l3,dif,xx5,x1,Nstrms,0,0)
      xx1(1) = 0.1
      xx1(2) = 0.5
      xx1(3) = 0.9
      DO j = 1 , Nstrms
         xx1(4) = xx3(j)
         xx1(5) = xx4(j)
         xx1(6) = xx5(j)
         x1 = (R(j,I)-R(1,I))/(R(Nstrms,I)-R(1,I))
         CALL alg01(xx1,xx1(4),3,x1,wpara(j),x1,1,0,0)
      ENDDO
      DO j = 1 , Nstrms
         xmr(j) = 0.0
         highm(j) = 0.0
         ang(j) = 0.0
         ws(j) = 0.0
         xinc(j) = 0.0
         beta1(j) = 0.0
         wd(j) = wpara(j)*2.0*sol(j)*sqrt(1.0+Tbeta(j,I)**2)
         wt(j) = wd(j)
      ENDDO
      IF ( Ndel(I)/=0 ) THEN
         l2 = Is3(I)
         CALL alg01(Delc(l2),Delta(l2),Ndel(I),xx2,pm1,x1,Nstrms,1,0)
         IF ( Ndata(l1)/=0 ) THEN
            CALL alg01(R(1,l1),X(1,l1),Nstrms,R(1,l1),x1,xx1,Nstrms,0,1)
            l2 = Ndimen(l1) + 1
            IF ( l2==2 ) THEN
               DO j = 1 , Nstrms
                  xx2(j) = R(j,l1)/R(j,Nstrms)
               ENDDO
            ELSEIF ( l2==3 ) THEN
               DO j = 1 , Nstrms
                  xx2(j) = Xl(j,l1)
               ENDDO
            ELSEIF ( l2==4 ) THEN
               DO j = 1 , Nstrms
                  xx2(j) = Xl(j,l1)/Xl(Nstrms,l1)
               ENDDO
            ELSE
               DO j = 1 , Nstrms
                  xx2(j) = R(j,l1)
               ENDDO
            ENDIF
            l2 = Is2(l1)
            l3 = Ndata(l1)
            CALL alg01(Datac(l2),Data1(l2),l3,xx2,xx3,x1,Nstrms,Nterp(l1),0)
            CALL alg01(Datac(l2),Data3(l2),l3,xx2,xx4,x1,Nstrms,Nterp(l1),0)
            DO j = 1 , Nstrms
               x1 = (atan((R(j,l1+1)-R(j,l1))/(X(j,l1+1)-X(j,l1)))+atan((R(j,l1)-R(j,l1-1))/(X(j,l1)-X(j,l1-1))))/2.0
               beta1(j) = atan((tan(xx3(j)/C1)*(1.0-xx1(j)*tan(x1))-tan(x1)*tan(xx4(j)/C1)*sqrt(1.0+xx1(j)**2))*cos(x1))
               xinc(j) = (atan(talph1(j))-beta1(j))*q
            ENDDO
         ENDIF
         DO j = 1 , Nstrms
            ang(j) = xinc(j) + pm1(j)/C1
            x1 = H(j,l1) - (Vm(j,l1)**2+Vw(j,l1)**2)/(2.0*G*Ej)
            IF ( x1<Hmin ) x1 = Hmin
            x4 = alg8(x1,S(j,l1))
            x2 = (x4+1.0)/(x4-1.0)
            x3 = sqrt(x2)
            x5 = alg9(x1,S(j,l1),Vm(j,l1)**2*(1.0+talph1(j)**2))
            xmr(j) = sqrt(x5)
            x6 = x5
            IF ( x6<1.0 ) x6 = 1.0
            x7 = x3*atan(sqrt(x6-1.0)/x3) - atan(sqrt(x6-1.0)) + ang(j)
            x10 = 0.0
            IF ( x7>0.0 ) THEN
               x8 = 0.4*Pi*(x3-1.0)
               IF ( x7>x8 ) THEN
                  x10 = sqrt(x6-1.0)
               ELSE
                  x9 = 1.0
                  k = 1
                  SPAG_Loop_2_1: DO
                     x10 = x9 - (x2+x9*x9)*(1.0+x9*x9)/(x9*x9*(x2-1.0))*(x3*atan(x9/x3)-atan(x9)-x7)
                     IF ( abs(x10-x9)<=0.00001 ) EXIT SPAG_Loop_2_1
                     IF ( k>20 ) THEN
                        IF ( Iprint/=0 ) THEN
                           CALL alg03(Lnct,1)
                           WRITE (Log2,99003) Ipass , I , j
99003                      FORMAT (5X,4HPASS,I3,9H  STATION,I3,12H  STREAMLINE,I3,                                                  &
                                  &58H  PRANDTL-MEYER FUNCTION NOT CONVERGED - USE INLET MACH NO)
                        ENDIF
                        x10 = sqrt(x6-1.0)
                        EXIT SPAG_Loop_2_1
                     ELSE
                        k = k + 1
                        x9 = x10
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
            ENDIF
            highm(j) = sqrt(1.0+x10*x10)
            x1 = (highm(j)+sqrt(x6))/2.0
            IF ( x5<1.0 ) x1 = x1*sqrt(x5)
            IF ( x1>1.0 ) THEN
               x1 = x1*x1
               ws(j) = (((x4+1.0)*x1/((x4-1.0)*x1+2.0))**(x4/(x4-1.0))*((x4+1.0)/(2.0*x4*x1-x4+1.0))**(1.0/(x4-1.0))-1.0)           &
                     & /((1.0+(x4-1.0)/2.0*x5)**(x4/(1.0-x4))-1.0)
            ENDIF
            wt(j) = wd(j) + ws(j)
         ENDDO
      ENDIF
      IF ( Iprint==1 ) THEN
         IF ( Lnct+3>Npage ) THEN
            IF ( Iprtc/=0 ) WRITE (Log2,99004)
99004       FORMAT (1H1)
            Lnct = 4 + Nstrms
         ENDIF
         IF ( Iprtc==1 ) WRITE (Log2,99005)
99005    FORMAT (5X,                                                                                                                &
         &'STREAM  INLET   OUTLET  CASCADE   DIFF       LOSS   DIFFUSION  BLADE  INCIDENCE  EXPANSION INLET  EXPANDED SHOCK   TOTAL'&
        & ,/,5X,                                                                                                                    &
         &'-LINE   RADIUS  RADIUS  SOLIDITY  FACTOR  PARAMETER   LOSS     ANGLE    ANGLE      ANGLE    M.NO  MACH NO   LOSS   LOSS '&
        & ,/,2X)
         Lnct = Lnct + 3
         DO j = 1 , Nstrms
            x1 = beta1(j)*C1*q
            x2 = xinc(j)*C1
            x3 = ang(j)*C1
            IF ( Iprtc==1 ) WRITE (Log2,99006) j , R(j,l1) , R(j,I) , sol(j) , dif(j) , wpara(j) , wd(j) , x1 , x2 , x3 , xmr(j) ,  &
                                 & highm(j) , ws(j) , wt(j)
99006       FORMAT (I9,F10.3,F8.3,2F9.4,F10.5,F9.5,2F9.3,F10.3,F10.4,F8.4,F8.5,F9.5)
         ENDDO
      ELSE
         l2 = Is2(I)
         l3 = Nterp(I)
         l4 = Ndata(I)
         IF ( Nwork(I)>=5 ) CALL alg01(Datac(l2),Data6(l2),l4,xx6,xx5,x1,Nstrms,l3,0)
         CALL alg01(Datac(l2),Data1(l2),l4,xx6,xx1,x1,Nstrms,l3,0)
         CALL alg01(Datac(l2),Data4(l2),l4,xx6,xx4,x1,Nstrms,l3,0)
         CALL alg01(Datac(l2),Data3(l2),l4,xx6,xx3,x1,Nstrms,l3,0)
         Ndata(I) = Nstrms
         l2 = l2 - 1
         DO j = 1 , Nstrms
            k = l2 + j
            Datac(k) = xx6(j)
            IF ( Nwork(I)>=5 ) Data6(k) = xx5(j)
            Data1(k) = xx1(j)
            IF ( wt(j)>wmax ) wt(j) = wmax
            Data2(k) = wt(j)
            Data3(k) = xx3(j)
            Data4(k) = xx4(j)
            Data5(k) = sol(j)
         ENDDO
      ENDIF
   END SUBROUTINE spag_block_1
END SUBROUTINE alg09
