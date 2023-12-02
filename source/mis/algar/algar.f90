!*==algar.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE algar
   IMPLICIT NONE
   USE C_CONTRL
   USE C_UD300C
   USE C_UD3PRT
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(59,30) :: deltar
   INTEGER :: if , ifailk , j , k , kk , l1 , l2 , l3 , l4 , l5
   REAL , DIMENSION(59) :: pass
   REAL :: psmid , vmin , x1 , x2 , x3 , xn , xx
   REAL , DIMENSION(21) :: vmlold , vmold , xx1 , xx2 , xx3 , xx4
   EXTERNAL alg01 , alg02 , alg04 , alg05 , alg06 , alg07 , alg08 , alg09 , alg10 , alg11 , alg12 , alg25 , alg26 , alg3 , alg4 ,   &
          & alg5 , alg6
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!
!
!
         if = 0
         Log1 = Loq1
         Log2 = Loq2
         Log3 = Loq3
         Log5 = Loq5
         Log6 = Loq6
         IF ( Iprtc==1 ) WRITE (Log2,99001)
99001    FORMAT (1HT)
         Pi = 3.141592653589
         C1 = 180.0/Pi
         Hmin = 50.0
         vmin = 25.0
         IF ( Iprtc==1 ) WRITE (Log2,99002)
99002    FORMAT (1H1,37X,53HPROGRAM ALG - COMPRESSOR DESIGN - AERODYNAMIC SECTION,/,38X,53(1H*))
         Lnct = 2
         CALL alg02
         Icase = 1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Iprtc==1 ) WRITE (Log2,99003) Icase
99003    FORMAT (1H1,9X,20HOUTPUT FOR POINT NO.,I2,/,10X,22(1H*))
         Lnct = 2
         DO I = 1 , 30
            DO j = 1 , 59
               deltar(j,I) = 0.0
            ENDDO
         ENDDO
         IF ( .NOT.((Icase==1 .AND. Nread==1) .OR. (Icase>1 .AND. ifailk==0)) ) THEN
            IF ( Nsplit/=1 ) THEN
               l1 = Nspec(1)
               xx1(1) = 0.0
               DO k = 2 , l1
                  xx1(k) = xx1(k-1) + sqrt((Rstn(k)-Rstn(k-1))**2+(Xstn(k)-Xstn(k-1))**2)
               ENDDO
               x1 = 1.0/xx1(l1)
               DO k = 2 , l1
                  xx1(k) = xx1(k)*x1
               ENDDO
               DO k = 1 , 11
                  xx2(k) = float(k-1)*0.1
               ENDDO
               CALL alg01(xx1,Xstn,l1,xx2,xx3,x1,11,0,0)
               CALL alg01(xx1,Rstn,l1,xx2,xx4,x1,11,0,0)
               DO k = 2 , 11
                  xx1(k) = xx1(k-1) + sqrt((xx3(k)-xx3(k-1))**2+(xx4(k)-xx4(k-1))**2)
                  xx3(k-1) = (xx1(k)+xx1(k-1))*0.5
               ENDDO
               l2 = Is1(2)
               xx2(1) = atan2(Rstn(l2)-Rstn(1),Xstn(l2)-Xstn(1))
               l2 = l2 + Nspec(2) - 1
               xx2(2) = atan2(Rstn(l2)-Rstn(l1),Xstn(l2)-Xstn(l1))
               Xi(1) = 0.0
               Xi(2) = xx1(11)
               CALL alg01(Xi,xx2,2,xx3,Phi,x1,10,1,0)
               CALL alg01(Rstn,Xstn,l1,xx3,x1,Gama,10,0,1)
               xx3(1) = 0.0
               DO k = 2 , 11
                  xx3(k) = xx3(k-1) + cos(Phi(k-1)+atan(Gama(k-1)))*(xx4(k)+xx4(k-1))*(xx1(k)-xx1(k-1))
               ENDDO
               x1 = 1.0/xx3(11)
               x2 = 1.0/xx1(11)
               DO k = 2 , 11
                  xx1(k) = xx1(k)*x2
                  xx3(k) = xx3(k)*x1
               ENDDO
               x1 = 1.0/float(Itub)
               DO k = 1 , Nstrms
                  xx2(k) = float(k-1)*x1
               ENDDO
               CALL alg01(xx1,xx3,11,xx2,Delf,x1,Nstrms,1,0)
            ENDIF
            DO I = 1 , Nstns
               l1 = Is1(I)
               l2 = Nspec(I)
               xx1(1) = 0.0
               Vv(1) = 0.0
               DO k = 2 , l2
                  l3 = l1 + k - 1
                  Vv(k) = Vv(k-1) + sqrt((Rstn(l3)-Rstn(l3-1))**2+(Xstn(l3)-Xstn(l3-1))**2)
               ENDDO
               x1 = 1.0/Vv(l2)
               DO k = 2 , l2
                  xx1(k) = Vv(k)*x1
               ENDDO
               DO k = 1 , 11
                  xx2(k) = float(k-1)*0.1
               ENDDO
               CALL alg01(xx1,Xstn(l1),l2,xx2,xx3,x1,11,0,0)
               CALL alg01(xx1,Rstn(l1),l2,xx2,xx4,x1,11,0,0)
               DO k = 2 , 11
                  xx1(k) = xx1(k-1) + sqrt((xx3(k)-xx3(k-1))**2+(xx4(k)-xx4(k-1))**2)
                  Gama(k-1) = (xx4(k)+xx4(k-1))*0.5
                  xx3(k-1) = (xx1(k)+xx1(k-1))*0.5
               ENDDO
               IF ( I/=1 .AND. I/=Nstns ) THEN
                  l3 = Is1(I+1)
                  l4 = Is1(I-1)
                  l5 = l1
                  xx2(1) = (atan2(Rstn(l3)-Rstn(l5),Xstn(l3)-Xstn(l5))+atan2(Rstn(l5)-Rstn(l4),Xstn(l5)-Xstn(l4)))*0.5
                  l3 = l3 + Nspec(I+1) - 1
                  l4 = l4 + Nspec(I-1) - 1
                  l5 = l5 + l2 - 1
                  xx2(2) = (atan2(Rstn(l3)-Rstn(l5),Xstn(l3)-Xstn(l5))+atan2(Rstn(l5)-Rstn(l4),Xstn(l5)-Xstn(l4)))*0.5
               ELSEIF ( I==Nstns ) THEN
                  l4 = Is1(I-1)
                  xx2(1) = atan2(Rstn(l1)-Rstn(l4),Xstn(l1)-Xstn(l4))
                  l4 = l4 + Nspec(I-1) - 1
                  l3 = l1 + l2 - 1
                  xx2(2) = atan2(Rstn(l3)-Rstn(l4),Xstn(l3)-Xstn(l4))
               ELSE
                  l3 = Is1(2)
                  xx2(1) = atan2(Rstn(l3)-Rstn(1),Xstn(l3)-Xstn(1))
                  l4 = Nspec(1)
                  l3 = l3 + Nspec(2) - 1
                  xx2(2) = atan2(Rstn(l3)-Rstn(l4),Xstn(l3)-Xstn(l4))
               ENDIF
               Xi(1) = 0.0
               Xi(2) = xx1(11)
               CALL alg01(Xi,xx2,2,xx3,Phi,x1,10,1,0)
               CALL alg01(Rstn(l1),Xstn(l1),l2,Gama,x1,Gama,10,0,1)
               xx3(1) = 0.0
               DO k = 2 , 11
                  xx3(k) = xx3(k-1) + cos(Phi(k-1)+atan(Gama(k-1)))*(xx4(k)+xx4(k-1))*(xx1(k)-xx1(k-1))
               ENDDO
               x1 = 1.0/xx3(11)
               DO k = 2 , 11
                  xx3(k) = xx3(k)*x1
               ENDDO
               CALL alg01(xx3,xx1,11,Delf,Xl(1,I),x1,Nstrms,1,0)
               x1 = Vv(l2)/xx1(11)
               DO j = 2 , Nstrms
                  Xl(j,I) = Xl(j,I)*x1
               ENDDO
               CALL alg01(Vv,Xstn(l1),l2,Xl(1,I),X(1,I),x1,Nstrms,0,0)
               CALL alg01(Vv,Rstn(l1),l2,Xl(1,I),R(1,I),x1,Nstrms,0,0)
            ENDDO
         ENDIF
         IF ( Icase<=1 ) THEN
            x1 = (X(Imid,2)-X(Imid,1))**2 + (R(Imid,2)-R(Imid,1))**2
            Drdm2(1) = ((R(Nstrms,1)-R(1,1))**2+(X(Nstrms,1)-X(1,1))**2)/x1
            l1 = Nstns - 1
            DO I = 2 , l1
               x2 = (X(Imid,I+1)-X(Imid,I))**2 + (R(Imid,I+1)-R(Imid,I))**2
               x3 = x2
               IF ( x1<x3 ) x3 = x1
               Drdm2(I) = ((R(Nstrms,I)-R(1,I))**2+(X(Nstrms,I)-X(1,I))**2)/x3
               x1 = x2
            ENDDO
            Drdm2(Nstns) = ((R(Nstrms,Nstns)-R(1,Nstns))**2+(X(Nstrms,Nstns)-X(1,Nstns))**2)/x2
         ENDIF
         DO I = 1 , Nstns
            Wwbl(I) = Wblock(I)
         ENDDO
         Ipass = 1
         spag_nextblock_1 = 3
      CASE (3)
         I = 1
         IF ( .NOT.((Ipass>1 .OR. Icase>1) .AND. Ndata(1)==1) ) THEN
            l1 = Ndimen(1) + 1
            IF ( l1==2 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = R(j,1)/R(Nstrms,1)
               ENDDO
            ELSEIF ( l1==3 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = Xl(j,1)
               ENDDO
            ELSEIF ( l1==4 ) THEN
               DO j = 1 , Nstrms
                  xx1(j) = Xl(j,1)/Xl(Nstrms,1)
               ENDDO
            ELSE
               DO j = 1 , Nstrms
                  xx1(j) = R(j,1)
               ENDDO
            ENDIF
            l1 = Nterp(1)
            l2 = Ndata(1)
            CALL alg01(Datac,Data1,l2,xx1,S,x1,Nstrms,l1,0)
            CALL alg01(Datac,Data2,l2,xx1,H,x1,Nstrms,l1,0)
            CALL alg01(Datac,Data3,l2,xx1,Tbeta,x1,Nstrms,l1,0)
            DO j = 1 , Nstrms
               H(j,1) = alg6(S(j,1),H(j,1))
               S(j,1) = alg3(S(j,1),H(j,1))
               Tbeta(j,1) = tan(Tbeta(j,1)/C1)
            ENDDO
         ENDIF
         IF ( Ipass<=1 .AND. Icase<=1 ) THEN
            x1 = Flow(1)/(alg5(H,S)*Pi*(R(Nstrms,1)+R(1,1))*Xl(Nstrms,1))*Sclfac**2
            DO j = 1 , Nstrms
               Vm(j,1) = x1
            ENDDO
            IF ( Istag==1 ) Vm(1,1) = 0.0
         ENDIF
         Ifailo = 0
         Iffail = 0
         Ivfail = 0
         DO j = 1 , Nstrms
            vmold(j) = Vm(j,1)
         ENDDO
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
         DO j = 1 , Nstrms
            Vwkeep(j) = Vw(j,I-1)
            Skeep(j) = S(j,I-1)
            Hkeep(j) = H(j,I-1)
         ENDDO
         x1 = H(Imid,I-1) - (Vm(Imid,I-1)**2+Vw(Imid,I-1)**2)/(2.0*G*Ej)
         IF ( x1<Hmin ) x1 = Hmin
         psmid = alg4(x1,S(Imid,I-1))
         IF ( Nmix==1 ) CALL alg04(H(1,I-1),S(1,I-1),Vw(1,I-1),R(1,I-1),R(1,I),X(1,I-1),X(1,I),Vm(1,I-1),Conmx,Sclfac,G,Ej,Hmin,    &
                                 & vmin,psmid,Nstrms,Log2,Lnct,if)
         IF ( if/=0 ) THEN
            Ifailo = I - 1
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Nwork(I)==0 ) THEN
            DO j = 1 , Nstrms
               H(j,I) = H(j,I-1)
               S(j,I) = S(j,I-1)
               Vw(j,I) = 0.0
               IF ( I>Istag .OR. j/=1 ) Vw(j,I) = Vw(j,I-1)*Rim1(j)/R(j,I)
            ENDDO
         ELSE
            CALL alg05
            IF ( Ntrans==1 .AND. Ipass>1 ) CALL alg06(R(1,I-1),R(1,I),X(1,I-1),X(1,I),H(1,I),S(1,I),Vm(1,I),Tbeta(1,I-1),Tbeta(1,I),&
               & Loss,Contr,Sclfac,Speed(I),Spdfac(Icase),G,Ej,Hmin,Nstrms,Pi)
            Iter = 0
            CALL alg07
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         DO j = 1 , Nstrms
            vmlold(j) = Vm(j,I)
         ENDDO
         IF ( Neqn>=2 ) THEN
            CALL alg26
         ELSE
            CALL alg08
         ENDIF
         IF ( Neval(I)<=0 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Iprint = 0
         CALL alg09
         IF ( Ifailo/=0 .AND. Ipass>Nforce ) THEN
            spag_nextblock_1 = 7
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         DO j = 1 , Nstrms
            IF ( abs(Vm(j,I)/vmlold(j)-1.0)>Tolnce/5.0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         IF ( Iloss<Nliter(I) ) THEN
            Iloss = Iloss + 1
            DO j = 1 , Nstrms
               vmlold(j) = Vm(j,I)
            ENDDO
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         IF ( Ipass>Nforce ) THEN
            IF ( Lnct+1>Npage ) THEN
               IF ( Iprtc==1 ) WRITE (Log2,99004)
99004          FORMAT (1H1)
               Lnct = 1
            ENDIF
            Lnct = Lnct + 1
            x1 = Vm(1,I)/vmlold(1)
            x2 = Vm(Imid,I)/vmlold(Imid)
            x3 = Vm(Nstrms,I)/vmlold(Nstrms)
            IF ( Iprtc==1 ) WRITE (Log2,99005) Ipass , I , x1 , x2 , x3
99005       FORMAT (5X,4HPASS,I3,9H  STATION,I3,66H  VM PROFILE NOT CONVERGED WITH LOSS RECALC   VM NEW/VM PREV  HUB=,F9.6,6H  MID=,&
                  & F9.6,7H  CASE=,F9.6)
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         IF ( Nbl==1 .AND. (Ifailo==0 .OR. Ipass<=Nforce) ) CALL alg10
         DO j = 1 , Nstrms
            Xim1(j) = X(j,I)
            Rim1(j) = R(j,I)
            IF ( I/=Istag .OR. j/=1 ) THEN
               IF ( abs(Vm(j,I)/vmold(j)-1.0)>Tolnce ) Ivfail = Ivfail + 1
               IF ( abs(Delw(j)-Delf(j))>Tolnce ) Iffail = Iffail + 1
            ENDIF
         ENDDO
         IF ( .NOT.(Nmax==1 .OR. (Ipass==1 .AND. Nread==1)) ) THEN
            x1 = Fm2
            IF ( x1<1.0-Xmmax ) x1 = 1.0 - Xmmax
            x2 = 1.0
            IF ( I==1 .OR. Nwork(I)>=5 ) x2 = 1.0 + Tbeta(Imid,I)**2
            x1 = 1.0/(1.0+x1*Drdm2(I)/(Rconst*x2))
            l3 = Nstrms - 2
            CALL alg01(Delw,Xl(1,I),Nstrms,Delf(2),xx1(2),x1,l3,1,0)
            xx = Xl(Imid,I)
            DO j = 2 , Itub
               Xl(j,I) = Xl(j,I) + x1*(xx1(j)-Xl(j,I))
            ENDDO
            l1 = Ipass
            IF ( l1>59 ) THEN
               l1 = 59
               DO k = 1 , 58
                  deltar(k,I) = deltar(k+1,I)
               ENDDO
            ENDIF
            deltar(l1,I) = Xl(Imid,I) - xx
            l1 = Is1(I)
            l2 = Nspec(I)
            xx1(1) = 0.0
            DO k = 2 , l2
               kk = l1 - 1 + k
               xx1(k) = xx1(k-1) + sqrt((Xstn(kk)-Xstn(kk-1))**2+(Rstn(kk)-Rstn(kk-1))**2)
            ENDDO
            CALL alg01(xx1,Rstn(l1),l2,Xl(2,I),R(2,I),x1,l3,0,0)
            CALL alg01(xx1,Xstn(l1),l2,Xl(2,I),X(2,I),x1,l3,0,0)
         ENDIF
         IF ( Ipass<=Nforce .OR. Ifailo==0 ) THEN
            IF ( I/=Nstns ) THEN
               I = I + 1
               IF ( Ipass<=1 ) THEN
                  DO j = 1 , Nstrms
                     Vm(j,I) = Vm(j,I-1)
                  ENDDO
                  IF ( I-1==Istag ) Vm(1,I) = Vm(2,I)
                  IF ( I==Istag ) Vm(1,I) = 0.0
               ENDIF
               Iloss = 1
               DO j = 1 , Nstrms
                  vmold(j) = Vm(j,I)
               ENDDO
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Ipass<Nmax ) THEN
               IF ( Ifailo/=0 ) THEN
                  Ipass = Ipass + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( Ivfail/=0 .OR. Iffail/=0 ) THEN
                  Ipass = Ipass + 1
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 9
      CASE (9)
         CALL alg11
         l1 = Nstns
         IF ( Ifailo/=0 ) l1 = Ifailo
         Iprint = 1
         DO I = 2 , l1
            IF ( Neval(I)/=0 ) CALL alg09
         ENDDO
         IF ( Nplot/=0 ) CALL alg12
         IF ( Ifailo==0 ) THEN
            IF ( Npunch/=0 ) THEN
               WRITE (Log3,99006) (Delf(j),j=1,Nstrms)
99006          FORMAT (6F12.8)
               WRITE (Log3,99007) ((R(j,I),X(j,I),Xl(j,I),I,j,j=1,Nstrms),I=1,Nstns)
99007          FORMAT (3F12.8,2I3)
            ENDIF
            DO I = 1 , Nstns
               IF ( Nout1(I)/=0 ) THEN
                  WRITE (Log3,99008) (R(j,I),j,I,j=1,Nstrms)
99008             FORMAT (F12.8,60X,2I4)
               ENDIF
            ENDDO
            l1 = Log3
            IF ( Narbit/=0 ) l1 = Log6
            DO I = 1 , Nstns
               IF ( Nout2(I)/=0 ) THEN
                  l2 = Is1(I)
                  l3 = l2 + Nspec(I) - 1
                  WRITE (l1,99009) Nspec(I) , (Xstn(k),Rstn(k),k=l2,l3)
99009             FORMAT (I3,/,(2F12.7))
                  xn = Speed(I)
                  IF ( I/=Nstns ) THEN
                     IF ( Speed(I)/=Speed(I+1) .AND. Nwork(I+1)/=0 ) xn = Speed(I+1)
                  ENDIF
                  xn = xn*Spdfac(Icase)*Pi/(30.0*Sclfac)
                  DO j = 1 , Nstrms
                     xx1(j) = atan((Vw(j,I)-xn*R(j,I))/Vm(j,I))*C1
                  ENDDO
                  WRITE (l1,99010) (R(j,I),xx1(j),j,I,j=1,Nstrms)
99010             FORMAT (2F12.8,48X,2I4)
               ENDIF
            ENDDO
         ENDIF
         IF ( Nstplt/=0 ) THEN
            l1 = Ipass
            IF ( l1>59 ) l1 = 59
            DO k = 1 , l1
               pass(k) = float(k)
            ENDDO
            DO k = 1 , Nstns
               IF ( Iprtc==1 ) WRITE (Log2,99011) k
99011          FORMAT (1H1,53X,19HDELTA L FOR STATION,I3,/,2X)
               CALL alg25(l1,Ipass,Log2,pass,deltar(1,k))
            ENDDO
         ENDIF
         IF ( Icase>=Ncase ) THEN
            IF ( Iprtc==1 ) WRITE (Log2,99012)
99012       FORMAT (1HS)
         ELSE
            Icase = Icase + 1
            ifailk = Ifailo
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE algar
